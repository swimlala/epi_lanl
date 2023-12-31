CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:27Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z           9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112547  20190522121836  1901_5055_016                   2C  D   APEX                            2140                            040306                          846 @�Q/���	1   @�Q0W:�@0I�^5?�c~vȴ9X1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@���@���A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   BffB  B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�33B�  B���B���B���B�  B�  B���B�  B�  B���B���B�  B�  B�  B�33B�  B���B���B�  C   C  C�fC�fC�fC	�fC�fC�fC  C�C�C  C  C�C  C�fC   C"�C$  C%�fC(  C*  C+�fC.  C0  C2  C3�fC5�fC8  C:�C<  C>  C@  CB  CD�CF  CG�fCJ  CL�CN  CP  CR  CT  CV�CX  CZ  C\�C^�C`  Cb  Cd  Cf�Ch  Cj  Cl�Cn  Co�fCr  Ct�Cv  Cw�fCy�fC{�fC}�fC�fC��3C��3C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��C��3C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C��C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C��3D y�D  D�fDfD�fD  D� D  D�fD  D� DfD� D  D� D  D� D	  D	� D
  D
y�D  D� D  D� D  D�fD  D� DfD� D��D� DfD� D��D� DfD� D  D� DfD� D  D�fDfD� D  D� D��D� D  D� D  D� D  D� D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$y�D$��D%�fD&  D&� D'  D'�fD(fD(� D)  D)� D*  D*� D*��D+y�D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3fD3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9  D9� D:  D:y�D;  D;� D;��D<� D=  D=� D=��D>y�D>��D?y�D@fD@� DA  DA�fDB  DB� DC  DC� DD  DD� DEfDE� DE��DF� DG  DG� DG��DH� DIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDN  DNy�DO  DO� DPfDP�fDQfDQ� DR  DR�fDS  DSy�DS��DT� DU  DU�fDV  DVy�DW  DW� DX  DXy�DY  DY� DZfDZ� DZ��D[� D\  D\� D\��D]� D^fD^� D_  D_� D`  D`� DafDa�fDb  Db� Dc  Dc� Dd  Dd� De  De�fDffDf� Df��Dgy�Dh  Dh� Di  Di�fDj  Djy�Dj��Dk� Dl  Dl� Dm  Dm�fDnfDn�fDo  Do� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds� DtfDt� Dt��Du� Dv  Dv� Dy� D�3D�<�D�@ D�ɚD�� D�0 D�c3D���D���D�&fD���D��3D��3D�33Dڌ�D��D��fD�  D�<�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@y��@�ff@�ffA��A<��A\��A|��A�ffA�ffA�ffA�ffA�33A�ffA�ffA�ffB��B33B33B33B'33B/33B733B?33BF��BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B�ffB�ffB�ffBÙ�BǙ�B�ffBϙ�Bә�B�ffB�ffBߙ�B㙚B癚B���BB�ffB�ffB���B���C��C�3C�3C�3C	�3C�3C�3C��C�fC�fC��C��C�fC��C�3C��C!�fC#��C%�3C'��C)��C+�3C-��C/��C1��C3�3C5�3C7��C9�fC;��C=��C?��CA��CC�fCE��CG�3CI��CK�fCM��CO��CQ��CS��CU�fCW��CY��C[�fC]�fC_��Ca��Cc��Ce�fCg��Ci��Ck�fCm��Co�3Cq��Cs�fCu��Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC��fC��3C��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��3C��fC��fC��3C�ٚC�ٚC�ٚC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC��fC��3C��fC��fC��fC��fC��3C�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC��fC��fC��fC��fC�ٚC�ٚC��fC��fC��fC�ٚC��fC��fC��fC�ٚC��fC��fC��fC�ٚC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC�ٚC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC�ٚC��fC��fC��fC�ٚD l�D �3Dy�D��Dy�D�3Ds3D�3Dy�D�3Ds3D��Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
l�D
�3Ds3D�3Ds3D�3Dy�D�3Ds3D��Ds3D��Ds3D��Ds3D��Ds3D��Ds3D�3Ds3D��Ds3D�3Dy�D��Ds3D�3Ds3D��Ds3D�3Ds3D�3Ds3D�3Ds3D��Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#��D$l�D$��D%y�D%�3D&s3D&�3D'y�D'��D(s3D(�3D)s3D)�3D*s3D*��D+l�D+�3D,y�D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2y�D2��D3y�D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8y�D8�3D9s3D9�3D:l�D:�3D;s3D;��D<s3D<�3D=s3D=��D>l�D>��D?l�D?��D@s3D@�3DAy�DA�3DBs3DB�3DCs3DC�3DDs3DD��DEs3DE��DFs3DF�3DGs3DG��DHs3DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM�3DNl�DN�3DOs3DO��DPy�DP��DQs3DQ�3DRy�DR�3DSl�DS��DTs3DT�3DUy�DU�3DVl�DV�3DWs3DW�3DXl�DX�3DYs3DY��DZs3DZ��D[s3D[�3D\s3D\��D]s3D]��D^s3D^�3D_s3D_�3D`s3D`��Day�Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Dey�De��Dfs3Df��Dgl�Dg�3Dhs3Dh�3Diy�Di�3Djl�Dj��Dks3Dk�3Dls3Dl�3Dmy�Dm��Dny�Dn�3Dos3Do�3Dpy�Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds��Dts3Dt��Dus3Du�3Dvs3Dys3D��D�6fD�9�D��3D��D�)�D�\�D��fD��3D�  D��3DǼ�D���D�,�DچfD�fD�� D���D�6f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A��
A��A��
A��#A��#A��;A��HA��TA��HA��TA��`A��mA��mA��mA��yA��A��A��A��A��A��A��A��mA��mA��A��A��A��A���A��A��TA��#A��/A��yA��A��A���A�Q�A��A�33A�`BA��RA���A�33A��RA�oA�=qA���A�M�A���A��^A�A�$�A�A�bA�1A�  A��A��#A��A���A���A��mA�oA��A�=qA���A��A��hA�VA�t�A�M�A�z�A~JAy�
At�+Aq33AoC�An�Ah��Ad�`A_��AZ�yAVA�AR(�AOoAJ��AFM�AA�A@I�A?dZA;p�A9\)A8n�A7O�A5�;A5�A4��A4�+A5|�A7/A6�A6�DA69XA6r�A6�HA5�^A4�A4�A4(�A1S�A09XA.ffA+�A+hsA+C�A+��A+�^A-7LA.�A.�jA/x�A.��A,VA*�yA(��A(=qA'��A&��A$ȴA$~�A#A#�A"�jA"  A ȴA��A�Az�A�At�A33AK�A��AƨA�-A/A�mA\)A��A��A"�A+A`BAVA-A`BA�yA�yAK�A�HA��A��An�A1'A��A|�A7LA�DA�AO�AAr�A��A�A��A�AĜA$�A�
A��Ap�AȴA�mAp�A"�A
�A
I�A
bA
  A	�#A	�wA	�A	?}A	?}A��Ar�AZAE�AffAQ�A�yA��AA�AJA�wA�A�A��A��A��A�\A��AĜAĜA�RA��At�@�33@�x�@��!@���@�5?@�;d@�+@���@��@��^@�I�@�S�@��@�+@��#@�@�r�@�1@�;d@���@��y@��@���@�ȴ@�7L@�ƨ@�@��@�S�@�ȴ@��T@�p�@�@�Q�@��
@�+@���@��@�G�@�D@�A�@�  @�P@�o@�5?@�7L@�@��u@�I�@�ƨ@��@�^5@�=q@�@���@�7L@��`@���@ܣ�@�bN@�I�@��@��;@�v�@�hs@�`B@�/@��@�r�@���@��@թ�@�O�@���@�r�@�1@���@Ӆ@�\)@��y@ҧ�@�~�@�=q@��@�@Ѳ-@�`B@��`@�z�@�1@�t�@��@�V@Ͳ-@̼j@˥�@�33@�ƨ@̣�@�z�@�I�@�A�@��@�33@���@ʗ�@ʟ�@ʗ�@ʏ\@ʏ\@ʇ+@�V@��@��@ɲ-@�x�@�V@�r�@�A�@Ǿw@�t�@��y@�^5@�@ļj@�Z@�Q�@�1'@��@��m@öF@�t�@�"�@�@�ȴ@�V@�O�@�A�@��w@���@��@�7L@�Ĝ@��@��@�bN@��;@��@��\@��#@�%@���@��@� �@���@��\@�^5@�x�@���@�Z@��@�\)@�C�@��H@�5?@�@��`@�Q�@���@��;@�t�@�~�@�E�@��@��h@���@�x�@�^5@��+@�V@��7@���@�/@��j@�Z@�b@�  @��@���@�+@���@�v�@�$�@���@�%@�z�@��F@�33@���@���@��\@�n�@�^5@�M�@�-@�J@��@��^@��h@�O�@�O�@���@�j@�S�@��H@�=q@�`B@�?}@�7L@�/@��j@�j@�1'@���@���@�S�@�"�@��@��@���@��@�p�@�/@���@�z�@��
@�K�@�ȴ@�@�O�@��@��j@�Q�@���@��@��H@��\@�ff@�@��@��#@���@��7@�p�@�7L@���@���@�z�@�I�@�9X@��@��@��
@�S�@��y@��R@�7L@�V@�7L@}V@p�@gl�@^V@V�R@OK�@F��@?�w@8��@3��@,Z@&ff@  �@�F@l�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A��
A��A��
A��#A��#A��;A��HA��TA��HA��TA��`A��mA��mA��mA��yA��A��A��A��A��A��A��A��mA��mA��A��A��A��A���A��A��TA��#A��/A��yA��A��A���A�Q�A��A�33A�`BA��RA���A�33A��RA�oA�=qA���A�M�A���A��^A�A�$�A�A�bA�1A�  A��A��#A��A���A���A��mA�oA��A�=qA���A��A��hA�VA�t�A�M�A�z�A~JAy�
At�+Aq33AoC�An�Ah��Ad�`A_��AZ�yAVA�AR(�AOoAJ��AFM�AA�A@I�A?dZA;p�A9\)A8n�A7O�A5�;A5�A4��A4�+A5|�A7/A6�A6�DA69XA6r�A6�HA5�^A4�A4�A4(�A1S�A09XA.ffA+�A+hsA+C�A+��A+�^A-7LA.�A.�jA/x�A.��A,VA*�yA(��A(=qA'��A&��A$ȴA$~�A#A#�A"�jA"  A ȴA��A�Az�A�At�A33AK�A��AƨA�-A/A�mA\)A��A��A"�A+A`BAVA-A`BA�yA�yAK�A�HA��A��An�A1'A��A|�A7LA�DA�AO�AAr�A��A�A��A�AĜA$�A�
A��Ap�AȴA�mAp�A"�A
�A
I�A
bA
  A	�#A	�wA	�A	?}A	?}A��Ar�AZAE�AffAQ�A�yA��AA�AJA�wA�A�A��A��A��A�\A��AĜAĜA�RA��At�@�33@�x�@��!@���@�5?@�;d@�+@���@��@��^@�I�@�S�@��@�+@��#@�@�r�@�1@�;d@���@��y@��@���@�ȴ@�7L@�ƨ@�@��@�S�@�ȴ@��T@�p�@�@�Q�@��
@�+@���@��@�G�@�D@�A�@�  @�P@�o@�5?@�7L@�@��u@�I�@�ƨ@��@�^5@�=q@�@���@�7L@��`@���@ܣ�@�bN@�I�@��@��;@�v�@�hs@�`B@�/@��@�r�@���@��@թ�@�O�@���@�r�@�1@���@Ӆ@�\)@��y@ҧ�@�~�@�=q@��@�@Ѳ-@�`B@��`@�z�@�1@�t�@��@�V@Ͳ-@̼j@˥�@�33@�ƨ@̣�@�z�@�I�@�A�@��@�33@���@ʗ�@ʟ�@ʗ�@ʏ\@ʏ\@ʇ+@�V@��@��@ɲ-@�x�@�V@�r�@�A�@Ǿw@�t�@��y@�^5@�@ļj@�Z@�Q�@�1'@��@��m@öF@�t�@�"�@�@�ȴ@�V@�O�@�A�@��w@���@��@�7L@�Ĝ@��@��@�bN@��;@��@��\@��#@�%@���@��@� �@���@��\@�^5@�x�@���@�Z@��@�\)@�C�@��H@�5?@�@��`@�Q�@���@��;@�t�@�~�@�E�@��@��h@���@�x�@�^5@��+@�V@��7@���@�/@��j@�Z@�b@�  @��@���@�+@���@�v�@�$�@���@�%@�z�@��F@�33@���@���@��\@�n�@�^5@�M�@�-@�J@��@��^@��h@�O�@�O�@���@�j@�S�@��H@�=q@�`B@�?}@�7L@�/@��j@�j@�1'@���@���@�S�@�"�@��@��@���@��@�p�@�/@���@�z�@��
@�K�@�ȴ@�@�O�@��@��j@�Q�@���@��@��H@��\@�ff@�@��@��#@���@��7@�p�@�7L@���@���@�z�@�I�@�9X@��@��@��
@�S�@��y@��R@�7L@�V@�7L@}V@p�@gl�@^V@V�R@OK�@F��@?�w@8��@3��@,Z@&ff@  �@�F@l�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�oB
�7B
�PB
�jB
�-B
�-B
�bB
z�B
y�B
�^B
��B%BBA�Bl�BdZB\)B_;B�B�9B�)B�mB��B�B�fB��B�}B�-B��B�3B�B�DBhsBF�B
�NB
jB
  B	��B	��B	�\B	u�B	`BB	L�B	=qB	�B��B�5B�wB�!B��B��B��B��B�'B�-B�BBɺB��B�B�yB��B��B	VB	>wB	{�B	�B	�DB	��B	�dB	��B	�B	�HB	�yB	�NB	��B	ÖB	�^B	�B	�3B	B	��B	�B
B
!�B
(�B
7LB
/B
�B
\B	��B	��B	��B	�B	�yB	�sB	�yB	�B	�B	�B	�yB	�fB	�ZB	�TB	�TB	�ZB	�mB	�B	��B
B
B
B
B
B
B
  B	��B
	7B
hB
VB
DB
JB
PB
bB
�B
�B
�B
 �B
!�B
 �B
!�B
$�B
"�B
!�B
 �B
"�B
 �B
�B
�B
�B
�B
�B
uB
bB
\B
bB
�B
uB
oB
bB
\B
\B
oB
{B
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
1'B
49B
6FB
5?B
49B
/B
/B
/B
0!B
2-B
0!B
(�B
�B
bB
oB
{B

=B	��B	��B	�B	�B	��B
B
B
B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�mB	�fB	�`B	�ZB	�ZB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
+B
+B
+B
+B
%B
%B
%B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
+B
+B
1B
1B
1B
1B
1B
1B
1B
1B
DB
DB
DB
PB
PB
JB
JB
JB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
PB
DB

=B
	7B
1B
+B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B
	7B
DB
JB
PB
PB
JB
JB
JB
DB
JB
PB
VB
VB
VB
bB
hB
hB
hB
�B
"�B
,B
2-B
=qB
C�B
H�B
M�B
T�B
ZB
_;B
bNB
gmB
jB
o�B
s�B
w�B
{�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
�B
�B
�oB
�B
��B{B�B#�BbNBz�Br�BjB`BB�B�?B�;B�B��B��BB�B��BĜB��B��B��B��B�7B�=B(�B
�B
&�B	��B	ǮB	�B	�DB	o�B	]/B	cTB	:^B	!�B	B�/B��B��B��B�}B�qB�}B��BȴB��B��B�
B�;B�B��B��B	%B	6FB	}�B	�1B	�JB	��B	�jB	�
B	�BB	�fB	�B	��B	�B	��B	ȴB	�B	�3B	�wB	��B	��B	��B
 �B
'�B
@�B
?}B
)�B
�B
B
B
%B	��B	�B	�B	�B	��B	��B	��B	�B	�B	�fB	�mB	�fB	�`B	�fB	�B	��B
B
+B
	7B
+B
%B
	7B
B	��B
	7B
�B
{B
bB
VB
PB
\B
�B
�B
 �B
"�B
#�B
#�B
$�B
'�B
'�B
%�B
%�B
%�B
%�B
%�B
"�B
�B
�B
�B
�B
oB
hB
{B
�B
�B
�B
uB
oB
hB
{B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
49B
7LB
8RB
8RB
8RB
0!B
0!B
0!B
1'B
5?B
7LB
33B
(�B
oB
�B
�B
�B
%B
  B	�B	�B	�B
B
	7B
JB
B	��B	�B	�B	�B	�B	�B	��B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�mB	�sB	�sB	�mB	�mB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
B
B
B
%B
B
B
B
%B
B
+B

=B
1B
+B
1B
1B
+B
+B
+B
+B
1B
1B
1B

=B
	7B
+B
+B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
B
+B
1B
+B
+B
1B
1B
1B
	7B
	7B
1B
1B
1B

=B
DB
PB
DB
PB
PB
PB
PB
PB
PB
VB
PB
PB
VB
\B
\B
\B
\B
bB
bB
VB
VB
VB
DB

=B
DB

=B
	7B
1B
	7B
	7B
	7B

=B
DB
DB
DB

=B

=B
	7B
JB
JB
PB
PB
PB
PB
JB
JB
JB
PB
VB
\B
VB
hB
oB
hB
hB
�B
"�B
,B
2-B
=qB
C�B
H�B
M�B
T�B
ZB
_;B
bNB
gmB
jB
o�B
s�B
w�B
{�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
=C�=��P<�1<ě�=��<�`B<�j<#�
<��
<�1<���<�=o<e`B<e`B<e`B<#�
<#�
<#�
<#�
<#�
<#�
<49X<�`B<49X<T��<�t�<D��<e`B<�1<ě�=o=�+=�C�=��=��=0 �<��<�<�1<u<�o=�P=o=�P=C�<�<�h<���=o=+<�/<T��<u<���<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��
<49X<e`B<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250122012011312501220120113125012  AO  ARGQ                                                                        20111205112547  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112547  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125013  IP                  G�O�G�O�G�O�                