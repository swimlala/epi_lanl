CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:46Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143740  20190522121828  1728_5048_026                   2C  D   APEX                            2142                            040306                          846 @�kw����1   @�kx@y@@5���v��c?�l�C�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF�CH  CJ  CL  CM�fCP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  DvffDyFfD�	�D�0 D�&fD�ɚD��3D�)�D���D��fD���D�33D�s3Dǩ�D���D�FfD�y�D�fD��D�&fD�VfD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @y��@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;�3C=��C?��CA��CC��CE�fCG��CI��CK��CM�3CO��CQ��CS��CU��CW�fCY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC�ٚC�ٚC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	��D
s3D
�3Ds3D��Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)��D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK��DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ��DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfy�Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3DvY�Dy9�D�3D�)�D�  D��3D���D�#3D��3D�� D��3D�,�D�l�Dǣ3D��3D�@ D�s3D� D��3D�  D�P D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��A���A���A��A�|�A�z�A�x�A�C�A��!A�&�A��A�v�A�dZA�\)A�Q�A�E�A�7LA��A�JA�VA���A�A���A�+A���A�"�A��A�jA�Q�A�&�A��A��;A�x�A�Q�A���A���A�r�A�1'A��A��^A��A�A�A���A�z�A��A���A�1A�$�A�hsA�|�A�ȴA�bA���A��FA�G�A�7LA�5?A�dZA�ffA���A��HA�\)A���A��
A�A�C�A���A� �A�VA��RA�A�ƨA�bNA�r�A�1A��-A�ƨA�bA�hsA�
=A���A�x�A���A��A�jA��uA��A���A�\)A�n�A�`BA�/A�M�A�
=A�hsA���A�ƨA33A~5?A}��A|��A{7LAyx�Aw�^At��Arn�Ap�An��Al�yAi�Ahv�Af��Ad�AbȴA`�uA^��A]�;A]`BA]&�A\-A[�AZr�AY/AT��AS?}AR^5AP�9AO+AN�DAM`BAK�#AJz�AI`BAG��AG
=AF�uAF5?AE;dAD�AC�FAB�ABjAA��A@�A?��A?O�A>��A=�PA;�;A9ƨA8��A8z�A8�A7C�A6�DA61'A5�A41A3\)A2JA17LA0I�A/�A.�jA.$�A-�;A-|�A-/A,��A+�TA+"�A*A(n�A(=qA'�
A'��A&��A%A#�A"��A"=qA!\)A ��A �DA -A7LA�A��A/A~�AhsAM�A/A��Ar�A��AA��A�A�uA  A33A�FAz�A  A�A��A	�hA/AA�A��A9XA-A1A��A~�A�FA �@���@��\@�K�@�G�@�(�@�-@�I�@�v�@��-@�r�@ꟾ@��/@�w@�R@��`@�v�@���@��
@��T@۶F@ڰ!@ّh@��`@�z�@�(�@�b@���@�l�@���@�`B@Դ9@�I�@Ӿw@�C�@�-@У�@��@��@�Ĝ@ʸR@�7L@�I�@�ƨ@�t�@��@��/@�b@î@��@+@�E�@���@���@��#@�hs@��@���@��h@�Q�@�|�@���@���@��!@���@�&�@���@� �@��F@�|�@�;d@��@�~�@��@�X@�%@�z�@���@�l�@�;d@��y@���@��T@���@���@��w@���@�\)@���@�{@��^@�`B@��`@�ƨ@�\)@��@�o@���@�p�@�O�@�9X@�1@���@�1'@��
@�/@�33@�M�@��;@�@�C�@��@���@��\@��\@���@��y@�|�@�Z@�z�@�z�@�I�@��
@�"�@�ȴ@���@��`@�Ĝ@�Ĝ@�(�@�  @��;@�ƨ@��P@��P@�Q�@��D@�9X@�1@��@�@��H@��!@��T@�E�@�=q@�x�@�b@��y@��#@�%@���@�Z@�1@��@�t�@�|�@���@���@�@�M�@�X@� �@���@��R@���@��!@���@���@�x�@��@�%@��/@�r�@���@�ff@�M�@�{@���@���@��h@�`B@�`B@�`B@��@���@��u@�1@�t�@�"�@�@�n�@�-@�V@��@�t�@��H@���@�~�@�5?@��@���@�/@�%@���@���@���@�%@���@��R@���@�7L@�x�@��@���@��u@���@��u@��@��@���@���@���@�|�@�S�@�
=@�ȴ@���@�n�@�$�@���@�X@�A�@�C�@��H@��@�hs@��/@��
@�G�@�M�@���@��!@���@��y@���@��@��y@��@�@�"�@�@��R@�5?@���@�@��^@���@�x�@�O�@��j@�Q�@�(�@��@�l�@��H@���@�~�@�E�@���@�@��h@��@�x�@�b@��h@zn�@q��@j��@dI�@Z�!@R~�@M�h@E?}@=�T@7\)@/�;@)G�@%`B@�@(�@�P@�H@{@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��TA��A���A���A��A�|�A�z�A�x�A�C�A��!A�&�A��A�v�A�dZA�\)A�Q�A�E�A�7LA��A�JA�VA���A�A���A�+A���A�"�A��A�jA�Q�A�&�A��A��;A�x�A�Q�A���A���A�r�A�1'A��A��^A��A�A�A���A�z�A��A���A�1A�$�A�hsA�|�A�ȴA�bA���A��FA�G�A�7LA�5?A�dZA�ffA���A��HA�\)A���A��
A�A�C�A���A� �A�VA��RA�A�ƨA�bNA�r�A�1A��-A�ƨA�bA�hsA�
=A���A�x�A���A��A�jA��uA��A���A�\)A�n�A�`BA�/A�M�A�
=A�hsA���A�ƨA33A~5?A}��A|��A{7LAyx�Aw�^At��Arn�Ap�An��Al�yAi�Ahv�Af��Ad�AbȴA`�uA^��A]�;A]`BA]&�A\-A[�AZr�AY/AT��AS?}AR^5AP�9AO+AN�DAM`BAK�#AJz�AI`BAG��AG
=AF�uAF5?AE;dAD�AC�FAB�ABjAA��A@�A?��A?O�A>��A=�PA;�;A9ƨA8��A8z�A8�A7C�A6�DA61'A5�A41A3\)A2JA17LA0I�A/�A.�jA.$�A-�;A-|�A-/A,��A+�TA+"�A*A(n�A(=qA'�
A'��A&��A%A#�A"��A"=qA!\)A ��A �DA -A7LA�A��A/A~�AhsAM�A/A��Ar�A��AA��A�A�uA  A33A�FAz�A  A�A��A	�hA/AA�A��A9XA-A1A��A~�A�FA �@���@��\@�K�@�G�@�(�@�-@�I�@�v�@��-@�r�@ꟾ@��/@�w@�R@��`@�v�@���@��
@��T@۶F@ڰ!@ّh@��`@�z�@�(�@�b@���@�l�@���@�`B@Դ9@�I�@Ӿw@�C�@�-@У�@��@��@�Ĝ@ʸR@�7L@�I�@�ƨ@�t�@��@��/@�b@î@��@+@�E�@���@���@��#@�hs@��@���@��h@�Q�@�|�@���@���@��!@���@�&�@���@� �@��F@�|�@�;d@��@�~�@��@�X@�%@�z�@���@�l�@�;d@��y@���@��T@���@���@��w@���@�\)@���@�{@��^@�`B@��`@�ƨ@�\)@��@�o@���@�p�@�O�@�9X@�1@���@�1'@��
@�/@�33@�M�@��;@�@�C�@��@���@��\@��\@���@��y@�|�@�Z@�z�@�z�@�I�@��
@�"�@�ȴ@���@��`@�Ĝ@�Ĝ@�(�@�  @��;@�ƨ@��P@��P@�Q�@��D@�9X@�1@��@�@��H@��!@��T@�E�@�=q@�x�@�b@��y@��#@�%@���@�Z@�1@��@�t�@�|�@���@���@�@�M�@�X@� �@���@��R@���@��!@���@���@�x�@��@�%@��/@�r�@���@�ff@�M�@�{@���@���@��h@�`B@�`B@�`B@��@���@��u@�1@�t�@�"�@�@�n�@�-@�V@��@�t�@��H@���@�~�@�5?@��@���@�/@�%@���@���@���@�%@���@��R@���@�7L@�x�@��@���@��u@���@��u@��@��@���@���@���@�|�@�S�@�
=@�ȴ@���@�n�@�$�@���@�X@�A�@�C�@��H@��@�hs@��/@��
@�G�@�M�@���@��!@���@��y@���@��@��y@��@�@�"�@�@��R@�5?@���@�@��^@���@�x�@�O�@��j@�Q�@�(�@��@�l�@��H@���@�~�@�E�@���@�@��h@��@�x�@�b@��h@zn�@q��@j��@dI�@Z�!@R~�@M�h@E?}@=�T@7\)@/�;@)G�@%`B@�@(�@�P@�H@{@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�qB��B��B��BÖBĜBĜBǮB��B�/B�#B�/B�HB�`B�mB�sB�B�B�B��B��B��B+BVB8RBbNBz�B��B��B�B�-B�XB�}B��B��B�#B�;B�;B�HB�HB�;B�NB�HB�`B�TB�#B��BȴBǮB��B��B�9B�B��B��B��B��B��B��B��B��B�DB�B�Bs�BhsBaHBdZBaHBG�B-BbBB�B�BB��BÖB�XB��B�hB�%B�B�Bo�BB�B%�B�B
=B
�sB
��B
�wB
�B
��B
~�B
l�B
aHB
[#B
B�B
5?B
/B
-B
'�B
 �B
�B
oB	��B
  B	�B	�B	ǮB	�FB	�B	��B	�uB	�B	w�B	p�B	iyB	e`B	dZB	_;B	VB	S�B	E�B	7LB	'�B	�B	�B	hB		7B	B	B��B��B��B��B��B��B��B��B��B�B�B�B�B�sB�fB�TB�HB�NB��B��B��B��B��B��B��BǮBĜB��B�wB�dB�RB�FB�9B�3B�3B�RB�3B�B�B�B�B��B��B��B��B��B��B��B�bB�VB�PB�=B�=B�1B�%B�%B�B�B�B�B{�By�By�Bx�Bw�Bt�Bt�Bq�Bk�BgmBjBe`B^5B]/B[#BXBS�B]/BN�BH�BG�BF�BE�BE�BG�BB�BD�B:^B:^B>wB33B0!B0!B1'B/B+B+B/B-B,B+B-B(�B'�B#�B$�B#�B"�B"�B!�B"�B"�B!�B!�B �B �B!�B"�B"�B#�B"�B"�B&�B%�B'�B-B-B.B0!B2-B2-B5?B7LB7LB7LB8RB:^B?}BG�BL�BM�BM�BG�BE�BG�BF�BG�BP�BXB_;BdZBffBgmBjBl�Bm�Bo�Bq�Bs�Bv�Bz�B{�B}�B� B� B�B�B�7B�DB�7B�bB�uB��B��B��B��B��B��B��B�3B�LB�LB�qBĜBĜBȴBȴB��B��B�B�B�yB��B��B�B�B��B��B	  B	  B	B	1B	JB	�B	�B	!�B	"�B	%�B	(�B	$�B	#�B	$�B	(�B	.B	-B	2-B	2-B	2-B	2-B	5?B	8RB	;dB	I�B	L�B	L�B	O�B	R�B	VB	ZB	ZB	_;B	`BB	[#B	VB	S�B	R�B	Q�B	R�B	S�B	VB	VB	YB	\)B	`BB	dZB	n�B	o�B	o�B	iyB	ffB	cTB	dZB	hsB	iyB	jB	jB	m�B	u�B	x�B	� B	� B	~�B	� B	�B	�%B	�+B	�1B	�=B	�PB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�-B	�3B	�?B	�LB	�LB	�^B	�qB	�}B	��B	�
B	�B	�)B	�NB	�;B	�)B	�ZB	�fB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�ZB	�TB	�HB	�HB	�BB	�BB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
DB
�B
�B
"�B
(�B
49B
<jB
>wB
D�B
K�B
O�B
VB
[#B
aHB
dZB
iyB
m�B
q�B
u�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�wB��B��B��BÖBĜBĜBȴB��B�BB�/B�5B�HB�`B�mB�sB�B�B�B��B��B��B1BbB;dBe`B|�B��B��B�!B�3B�^B��B��B�
B�/B�BB�BB�NB�NB�NB�ZB�TB�fB�`B�;B�B��B��B��BĜB�RB�FB�'B��B��B��B��B��B��B��B�VB�7B�Bw�Bl�BcTBffBdZBN�B49B{B	7B��B�fB��BȴB��B��B�uB�+B�%B�%B|�BK�B+B�B{B
�B
�B
ĜB
�?B
��B
�%B
p�B
ffB
`BB
I�B
8RB
1'B
0!B
-B
%�B
#�B
�B
B
B	��B	�5B	��B	�XB	�'B	��B	��B	�=B	|�B	s�B	jB	ffB	gmB	bNB	XB	XB	P�B	;dB	+B	$�B	�B	uB	PB	+B	+B	B	B��B��B��B��B��B��B��B��B�B��B�B�yB�yB�mB�mB�yB�B��B��B�B��B��B��B��BǮBƨB��B�}B�dB�^B�FB�9B�9B�XB�9B�'B�B�B�B��B��B��B��B��B��B��B�oB�hB�\B�DB�DB�DB�1B�1B�+B�%B�B�B~�B{�Bz�Bz�Bz�Bv�Bv�Bu�Bs�BjBo�BiyB`BBaHB`BB_;B[#BaHBS�BJ�BG�BG�BG�BI�BJ�BE�BJ�BA�B?}BA�B5?B33B33B33B0!B-B.B2-B/B.B.B1'B+B)�B&�B'�B%�B$�B#�B"�B#�B"�B"�B"�B!�B#�B"�B#�B#�B$�B$�B%�B(�B'�B-B1'B0!B0!B1'B33B5?B7LB9XB8RB8RB9XB;dB@�BG�BM�BN�BP�BG�BH�BI�BH�BG�BP�BZBaHBe`BgmBhsBk�Bm�Bn�Bp�Br�Bt�Bw�B{�B|�B� B�B�B�B�%B�=B�PB�=B�hB�uB��B��B��B��B��B��B�B�9B�RB�LB�wBƨBŢB��BɺB��B��B�B�B�fB��B��B�B�B��B��B	B	  B	B	1B	DB	{B	�B	!�B	"�B	&�B	)�B	%�B	%�B	%�B	(�B	.B	.B	2-B	2-B	2-B	33B	5?B	7LB	;dB	J�B	M�B	M�B	P�B	R�B	W
B	[#B	ZB	_;B	aHB	]/B	XB	VB	S�B	R�B	S�B	T�B	W
B	W
B	YB	[#B	_;B	bNB	n�B	q�B	q�B	jB	gmB	cTB	dZB	iyB	jB	jB	k�B	m�B	u�B	y�B	�B	�B	~�B	�B	�B	�%B	�+B	�1B	�=B	�PB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�9B	�9B	�3B	�3B	�?B	�LB	�LB	�^B	�jB	�qB	ȴB	�
B	�B	�)B	�TB	�;B	�)B	�`B	�mB	�sB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�`B	�`B	�NB	�NB	�NB	�5B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
DB
�B
�B
"�B
(�B
49B
<jB
>wB
D�B
K�B
O�B
VB
[#B
bNB
dZB
iyB
n�B
q�B
u�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452022012011014520220120110145202  AO  ARGQ                                                                        20111130143740  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143740  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145202  IP                  G�O�G�O�G�O�                