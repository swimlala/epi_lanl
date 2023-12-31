CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:24Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112353  20190522121836  1901_5055_005                   2C  D   APEX                            2140                            040306                          846 @�5�33?�1   @�5���@0C�
=p��c+��-V1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B���B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-fD-�fD.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D?��D@y�D@��DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy��D��3D�0 D�vfD��3D�3D�9�D��3D��3D�� D�33D�c3D��fD���D�&fDچfD��D���D�	�D�<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @s33@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffAݙ�A�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo��Bw33B33B���B���B���B���B���B�ffB�ffB���B�ffB���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�B�ffBۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C�3C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy�fC{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dl�D��Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dl�D�3Ds3D��Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$y�D$�3D%s3D%�3D&s3D&�3D's3D'��D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,��D-y�D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5��D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?��D@l�D@��DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Dry�Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dy��D���D�)�D�p D���D���D�33D�|�D���D�ٚD�,�D�\�D�� D��fD�  Dڀ D�fD��fD�3D�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��;A��HA��TA��HA��/A��#A��#A��#A��#A��/A��;A��/A��HA��HA��HA��HA��TA��`A��mA��mA׾wA׸RA״9Aײ-Aו�A�jA�M�A�S�A��A�%A��`A�ȴA֧�A�C�Aպ^A�v�A��AͬAȺ^A��`A�/A��
A��HA��A�Q�A�O�A��TA�Q�A��HA��\A�5?A�|�A���A���A�hsA�JA�t�A��yA���A���A��/A�"�A��!A�C�A�33A��A�A�A�JA���A�K�A�
=A�l�A�C�A� �A�A���A�ƨA���A�/A�JA��HA�E�A�\)A�A�A���A�jA���A�x�A�M�A�ffA~�yA{hsAv  Ar1An(�AkVAf^5AaK�A`  A_�TA^�!AZ��AXbNAV��AS��AP��AN$�AM�PAMp�AJ  AFjACt�A>��A>��A?�A>VA;��A;�A;dZA:v�A9A4(�A1��A1%A0�jA-��A*��A*M�A(�+A$��A!�-A��A��AjAXA�A�HA  A��AffAVAx�AVA�uA��A��Az�A?}A�HAhsA�;AXAr�A��A�RA�A�A��A
=A�+A�AoAffA(�A�A��A��A�A{A��Ap�A
��AC�A�At�A`BAC�A
�A	�hA�A=qA��Ax�A"�A�uA�AK�AK�A��AA��A��A�A��A7LAl�A�HA��AG�A�HA��Az�AVA^5Av�AĜA�At�A`BA%A��A��A\)AdZA�A v�A E�@��mA �@��;@��@�v�@�O�@��@��;@��w@�+@�C�@�@�ff@�hs@�b@�"�@�hs@�9X@���@�@���@�O�@�I�@�~�@@�M�@�X@�9@�P@���@�r�@��
@�S�@��@�@�ȴ@�j@�r�@��@߮@�\)@���@ݩ�@�-@ޏ\@���@ޟ�@��@ܣ�@�A�@���@���@��@�%@ו�@��@�x�@ԣ�@��m@�(�@�\)@ӍP@�dZ@��@�v�@Ѻ^@�z�@�Z@϶F@�;d@�M�@͡�@̣�@�9X@��@˅@�
=@���@�n�@���@��@�r�@�  @���@Ɨ�@��@Ų-@��@���@�z�@�1@�C�@§�@�n�@�5?@��@�{@�{@��@��7@�V@�G�@��@�(�@���@�n�@��@��h@�`B@�/@��/@���@�bN@�  @��
@���@�"�@���@�@�hs@�V@�Z@���@��@�S�@��y@�ff@�$�@�J@���@��@�z�@�  @���@�+@���@��+@�ff@�E�@��T@�X@�Ĝ@�r�@�1'@��F@��@�-@��h@���@�j@��@��P@�33@��+@�=q@���@���@��-@��@���@��j@��D@��D@�Z@�  @�t�@���@�M�@���@��7@�?}@��`@���@��@�bN@�1@�l�@�C�@��@��!@��@�S�@�l�@��y@�ff@�M�@��T@�?}@���@��@��@�S�@�+@���@��@��!@�M�@�{@�=q@��@�/@��`@��@�z�@�1'@��m@�|�@�;d@�
=@���@�E�@��#@���@�X@���@�r�@�bN@���@�Ĝ@��/@���@�r�@�Z@�Q�@���@�C�@�dZ@�ȴ@�V@��@�$�@�V@�E�@�$�@���@��h@�hs@�?}@�%@���@�A�@�9X@�b@��m@���@�dZ@�33@��@��y@��R@�V@���@��-@�`B@�V@���@��@�Z@� �@��@��;@��
@���@�33@�ȴ@�~�@�~�@�ff@�{@���@��h@�x�@�?}@�/@�Ĝ@��@�bN@���@�5?@�ȴ@|��@uO�@i�7@^��@X  @Nȴ@F��@?��@9X@3"�@-O�@(b@!��@�@�`@�@�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��;A��HA��TA��HA��/A��#A��#A��#A��#A��/A��;A��/A��HA��HA��HA��HA��TA��`A��mA��mA׾wA׸RA״9Aײ-Aו�A�jA�M�A�S�A��A�%A��`A�ȴA֧�A�C�Aպ^A�v�A��AͬAȺ^A��`A�/A��
A��HA��A�Q�A�O�A��TA�Q�A��HA��\A�5?A�|�A���A���A�hsA�JA�t�A��yA���A���A��/A�"�A��!A�C�A�33A��A�A�A�JA���A�K�A�
=A�l�A�C�A� �A�A���A�ƨA���A�/A�JA��HA�E�A�\)A�A�A���A�jA���A�x�A�M�A�ffA~�yA{hsAv  Ar1An(�AkVAf^5AaK�A`  A_�TA^�!AZ��AXbNAV��AS��AP��AN$�AM�PAMp�AJ  AFjACt�A>��A>��A?�A>VA;��A;�A;dZA:v�A9A4(�A1��A1%A0�jA-��A*��A*M�A(�+A$��A!�-A��A��AjAXA�A�HA  A��AffAVAx�AVA�uA��A��Az�A?}A�HAhsA�;AXAr�A��A�RA�A�A��A
=A�+A�AoAffA(�A�A��A��A�A{A��Ap�A
��AC�A�At�A`BAC�A
�A	�hA�A=qA��Ax�A"�A�uA�AK�AK�A��AA��A��A�A��A7LAl�A�HA��AG�A�HA��Az�AVA^5Av�AĜA�At�A`BA%A��A��A\)AdZA�A v�A E�@��mA �@��;@��@�v�@�O�@��@��;@��w@�+@�C�@�@�ff@�hs@�b@�"�@�hs@�9X@���@�@���@�O�@�I�@�~�@@�M�@�X@�9@�P@���@�r�@��
@�S�@��@�@�ȴ@�j@�r�@��@߮@�\)@���@ݩ�@�-@ޏ\@���@ޟ�@��@ܣ�@�A�@���@���@��@�%@ו�@��@�x�@ԣ�@��m@�(�@�\)@ӍP@�dZ@��@�v�@Ѻ^@�z�@�Z@϶F@�;d@�M�@͡�@̣�@�9X@��@˅@�
=@���@�n�@���@��@�r�@�  @���@Ɨ�@��@Ų-@��@���@�z�@�1@�C�@§�@�n�@�5?@��@�{@�{@��@��7@�V@�G�@��@�(�@���@�n�@��@��h@�`B@�/@��/@���@�bN@�  @��
@���@�"�@���@�@�hs@�V@�Z@���@��@�S�@��y@�ff@�$�@�J@���@��@�z�@�  @���@�+@���@��+@�ff@�E�@��T@�X@�Ĝ@�r�@�1'@��F@��@�-@��h@���@�j@��@��P@�33@��+@�=q@���@���@��-@��@���@��j@��D@��D@�Z@�  @�t�@���@�M�@���@��7@�?}@��`@���@��@�bN@�1@�l�@�C�@��@��!@��@�S�@�l�@��y@�ff@�M�@��T@�?}@���@��@��@�S�@�+@���@��@��!@�M�@�{@�=q@��@�/@��`@��@�z�@�1'@��m@�|�@�;d@�
=@���@�E�@��#@���@�X@���@�r�@�bN@���@�Ĝ@��/@���@�r�@�Z@�Q�@���@�C�@�dZ@�ȴ@�V@��@�$�@�V@�E�@�$�@���@��h@�hs@�?}@�%@���@�A�@�9X@�b@��m@���@�dZ@�33@��@��y@��R@�V@���@��-@�`B@�V@���@��@�Z@� �@��@��;@��
@���@�33@�ȴ@�~�@�~�@�ff@�{@���@��h@�x�@�?}@�/@�Ĝ@��@�bN@���@�5?@�ȴ@|��@uO�@i�7@^��@X  @Nȴ@F��@?��@9X@3"�@-O�@(b@!��@�@�`@�@�71111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
k�B
k�B
k�B
k�B
jB
k�B
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
iyB
iyB
iyB
hsB
hsB
gmB
gmB
gmB
gmB
hsB
ffB
cTB
aHB
]/B
W
B
D�B
7LB
)�B
49B
_;B
�1B
��B
ÖB
ȴB
�`B�B-B;dBo�B�+B��B�LB�VB�bB��B�5B��BǮB��B�B�ZB�BĜB�RB�FB�jBƨB�XB�LB�B��B�PBe`B:^BJB
�B
��B
�3B
��B
x�B
gmB
]/B
>wB
,B
'�B
�B	��B	��B	��B	�XB	��B	�VB	l�B	YB	I�B	<jB	&�B	�B	\B	PB	B�B�`B�B��BB�wB��B�BȴB�B�-B��B��B�XBBƨB��B�ZB�`B�5BÖB��BƨB��B��BȴB��B��B�RB�B��B��B��B��B�BɺBɺB��B�B�B�B�B�B�B�B�B	1'B	[#B	ffB	q�B	w�B	}�B	w�B	o�B	w�B	|�B	v�B	w�B	y�B	u�B	n�B	_;B	ffB	e`B	W
B	YB	P�B	G�B	A�B	;dB	;dB	E�B	R�B	ZB	\)B	`BB	aHB	[#B	[#B	\)B	[#B	^5B	dZB	aHB	ZB	YB	_;B	e`B	dZB	cTB	k�B	n�B	q�B	w�B	|�B	x�B	u�B	w�B	v�B	x�B	{�B	|�B	~�B	�B	�JB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�!B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�!B	�3B	�-B	�'B	�B	�B	�B	�'B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�9B	�LB	�RB	�RB	�LB	�RB	�RB	�LB	�3B	�'B	�B	��B	��B	��B	��B	�B	�B	�B	�3B	�FB	�?B	�?B	�9B	�-B	�?B	�?B	�9B	�3B	�9B	�3B	�3B	�3B	�-B	�-B	�'B	�'B	�-B	�-B	�9B	�9B	�FB	�FB	�FB	�FB	�RB	�XB	�XB	�^B	�^B	�wB	�}B	��B	��B	B	ÖB	ÖB	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�#B	�)B	�#B	�5B	�/B	�)B	�/B	�/B	�/B	�5B	�HB	�HB	�HB	�HB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
B
B
B
+B

=B

=B
DB
DB
DB
DB
DB
JB
JB
JB
JB
JB
JB
PB
VB
VB
\B
\B
\B
bB
hB
hB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
,B
49B
7LB
@�B
I�B
K�B
Q�B
W
B
]/B
cTB
gmB
iyB
o�B
t�B
w�B
{�B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
k�B
k�B
k�B
k�B
jB
k�B
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
iyB
iyB
iyB
iyB
iyB
hsB
gmB
hsB
gmB
hsB
ffB
cTB
bNB
^5B
\)B
L�B
9XB
1'B
:^B
hsB
�uB
�B
��B
��B
�sB�B/B9XBp�B�JB��B�}B�uB��B��B�HB��BȴB�B�B�mB�/BɺB�jB�XB��B��B�}B��B�3B��B��Bo�BJ�B�B
��B
�;B
B
��B
� B
l�B
hsB
C�B
-B
+B
&�B
B	��B	ĜB	��B	�B	��B	u�B	bNB	Q�B	I�B	5?B	�B	bB	oB	bB��B�B�ZB�
B��B��B��B�ZB��B�RB�wB��B��B�dBǮBȴB��B�fB�yB�BɺBÖBǮB��B��BɺB��B�B��B�'B��B��B��B��B�B��B��B��B�B�B�B�B�B�B�B�B	-B	ZB	e`B	s�B	z�B	�B	}�B	o�B	w�B	�B	y�B	y�B	{�B	x�B	v�B	`BB	gmB	n�B	XB	\)B	T�B	K�B	E�B	<jB	:^B	D�B	R�B	ZB	]/B	bNB	cTB	]/B	\)B	]/B	\)B	_;B	ffB	cTB	[#B	YB	_;B	gmB	e`B	bNB	l�B	n�B	p�B	w�B	~�B	|�B	w�B	y�B	w�B	y�B	|�B	|�B	~�B	�B	�JB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�3B	�9B	�-B	�B	��B	��B	��B	��B	��B	�B	�!B	�B	��B	�B	�'B	�'B	�?B	�?B	�3B	�!B	�!B	�B	�-B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�9B	�LB	�XB	�^B	�RB	�RB	�RB	�XB	�9B	�3B	�!B	�B	��B	��B	��B	�B	�B	�B	�3B	�LB	�FB	�FB	�FB	�-B	�FB	�FB	�FB	�9B	�FB	�9B	�9B	�9B	�3B	�3B	�-B	�-B	�3B	�3B	�?B	�FB	�LB	�LB	�LB	�LB	�XB	�^B	�^B	�dB	�dB	�wB	��B	��B	��B	B	ÖB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�)B	�)B	�)B	�;B	�5B	�/B	�/B	�5B	�5B	�;B	�NB	�HB	�HB	�NB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
%B
B
+B
%B
B
B
+B

=B

=B
DB
JB
DB
DB
JB
PB
PB
JB
JB
JB
PB
VB
VB
VB
\B
\B
bB
hB
oB
oB
uB
{B
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
-B
49B
7LB
@�B
I�B
K�B
Q�B
W
B
]/B
cTB
gmB
iyB
p�B
t�B
w�B
{�B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<49X<D��<T��<u<T��<#�
<#�
<49X<#�
<#�
<#�
<49X<e`B<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<T��<e`B<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250092012011312500920120113125009  AO  ARGQ                                                                        20111205112353  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112353  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125009  IP                  G�O�G�O�G�O�                