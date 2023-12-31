CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:45Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143701  20190522121828  1728_5048_020                   2C  D   APEX                            2142                            040306                          846 @�\�M-�1   @�\���_�@4�ě��T�c2E����1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  D   D �fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	y�D
  D
y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  DwffDy� D�fD�33D�vfD�� D�  D�,�D�\�D��3D��3D�33D��fDǳ3D���D�  D�s3D�3D��D�  D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC�ٚC��fC��fC��fC��fD y�D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Dy�D�3D	l�D	�3D
l�D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��Dl�D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX��DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3DwY�Dys3D�  D�,�D�p D���D���D�&fD�VfD���D���D�,�D�� DǬ�D��fD��D�l�D��D��3D���D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��FA��RA��^A��jA��jA��^A�A�A���A��PA�z�A�r�A�n�A�jA�ffA�dZA�dZA�^5A�\)A�ZA�XA�Q�A�M�A�A�A�7LA�33A�1'A�/A�+A� �A�JA�  A���A���A�7LA�-A��A���A�bNA�M�A�G�A�1'A�+A��A��yA��wA��jA��FA���A���A��A�ffA�Q�A�C�A�&�A�oA��HA�`BA�{A��A��/A�A��A���A�33A��A���A��A���A��A�v�A���A��A�%A�oA��jA�G�A���A�v�A�K�A�9XA��A���A�A�A���A�C�A�ĜA��A�A�A�ZA�ĜA�VA��7A�jA���A���A��A�v�A�z�A�A�A���A���A�I�A��A�VA���A��`A��A���A�A�A�-A�l�A���A�+A�VA���A�1'A|�HAz��Az5?Ax��Au�PAtv�Asx�Ar��Aq�AodZAj�yAh5?Agt�AfffAe&�Ac��Aa�A]�A[�A["�AY�AX1AW`BAV��AVjAU�^AT~�ARv�AQG�AN�HAK|�AH��AE��AD5?AC��AChsAC�AB�jAA��AAS�A?��A=ƨA:A7��A6�/A6ffA5�A5"�A3?}A2z�A21'A2-A/��A.�yA-;dA+�A)�TA)��A)x�A)?}A(�HA'��A%t�A$A#�^A#+A"jA!�A v�AG�A�DA{Ax�Ap�A��A1'A�A�9A�!A�Ax�AĜA9XA�9A33A��AjA��A	33A(�A  A�A�uA{A`BAz�A\)A��AffA�A Q�@��w@���@��@�~�@�Ĝ@�@���@�`B@��@��H@�X@� �@�t�@�t�@�5?@���@��@ݩ�@�Ĝ@܋D@�b@ۮ@۝�@ڟ�@�M�@�E�@���@�`B@ؓu@�1'@ؓu@�&�@�p�@���@�z�@��y@�G�@���@���@���@���@�I�@Ӯ@�dZ@�n�@��@�%@� �@�\)@�ȴ@���@�@���@�hs@�(�@���@��#@ɩ�@�`B@�/@�V@��@ǶF@ƸR@�^5@���@���@���@�;d@�I�@��@���@ǥ�@�33@�ff@�&�@��`@�r�@�Q�@�1@�"�@�5?@�@���@�1@�n�@�x�@�X@��@�$�@�$�@��#@��7@��/@��@�I�@�(�@��
@�l�@���@��h@�/@���@��@��9@�%@���@���@���@�bN@���@�\)@�C�@�33@��@��H@��!@�M�@�?}@�%@��u@��@��;@��m@�33@�ff@��-@��h@�x�@�p�@�`B@�X@�G�@�V@���@��H@�33@��w@��@��
@���@��@�|�@�\)@��\@��#@�X@��j@�I�@�t�@�ȴ@��!@�n�@��@���@���@�@��7@�hs@���@�p�@�X@�/@��u@�9X@���@�dZ@�@��H@���@���@���@��R@�^5@�@��^@�G�@�%@��@�9X@��m@�|�@�+@��y@��\@��@��T@��^@�O�@��/@��u@�I�@��@�b@���@��@��y@�^5@�$�@���@��7@�?}@���@�z�@�  @�t�@�K�@�"�@���@�-@�@��7@��@���@���@��@�bN@��@���@�\)@��@���@��@��\@�^5@�=q@�{@��#@��-@���@�`B@�&�@�Ĝ@�b@��w@�l�@�33@�o@��@��!@���@�$�@��@�?}@��@��@���@��u@���@���@�Z@�j@�1'@�1@�  @��m@���@�;d@�
=@�
=@��@�=q@��T@�@���@�7L@��@�Ĝ@��@��D@�Q�@���@��@�S�@��@���@�-@�5?@�K�@|�@s�@l9X@b=q@Y��@R=q@G�w@Ax�@:=q@2��@-O�@'�w@"��@�R@�H@ȴ@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��FA��RA��^A��jA��jA��^A�A�A���A��PA�z�A�r�A�n�A�jA�ffA�dZA�dZA�^5A�\)A�ZA�XA�Q�A�M�A�A�A�7LA�33A�1'A�/A�+A� �A�JA�  A���A���A�7LA�-A��A���A�bNA�M�A�G�A�1'A�+A��A��yA��wA��jA��FA���A���A��A�ffA�Q�A�C�A�&�A�oA��HA�`BA�{A��A��/A�A��A���A�33A��A���A��A���A��A�v�A���A��A�%A�oA��jA�G�A���A�v�A�K�A�9XA��A���A�A�A���A�C�A�ĜA��A�A�A�ZA�ĜA�VA��7A�jA���A���A��A�v�A�z�A�A�A���A���A�I�A��A�VA���A��`A��A���A�A�A�-A�l�A���A�+A�VA���A�1'A|�HAz��Az5?Ax��Au�PAtv�Asx�Ar��Aq�AodZAj�yAh5?Agt�AfffAe&�Ac��Aa�A]�A[�A["�AY�AX1AW`BAV��AVjAU�^AT~�ARv�AQG�AN�HAK|�AH��AE��AD5?AC��AChsAC�AB�jAA��AAS�A?��A=ƨA:A7��A6�/A6ffA5�A5"�A3?}A2z�A21'A2-A/��A.�yA-;dA+�A)�TA)��A)x�A)?}A(�HA'��A%t�A$A#�^A#+A"jA!�A v�AG�A�DA{Ax�Ap�A��A1'A�A�9A�!A�Ax�AĜA9XA�9A33A��AjA��A	33A(�A  A�A�uA{A`BAz�A\)A��AffA�A Q�@��w@���@��@�~�@�Ĝ@�@���@�`B@��@��H@�X@� �@�t�@�t�@�5?@���@��@ݩ�@�Ĝ@܋D@�b@ۮ@۝�@ڟ�@�M�@�E�@���@�`B@ؓu@�1'@ؓu@�&�@�p�@���@�z�@��y@�G�@���@���@���@���@�I�@Ӯ@�dZ@�n�@��@�%@� �@�\)@�ȴ@���@�@���@�hs@�(�@���@��#@ɩ�@�`B@�/@�V@��@ǶF@ƸR@�^5@���@���@���@�;d@�I�@��@���@ǥ�@�33@�ff@�&�@��`@�r�@�Q�@�1@�"�@�5?@�@���@�1@�n�@�x�@�X@��@�$�@�$�@��#@��7@��/@��@�I�@�(�@��
@�l�@���@��h@�/@���@��@��9@�%@���@���@���@�bN@���@�\)@�C�@�33@��@��H@��!@�M�@�?}@�%@��u@��@��;@��m@�33@�ff@��-@��h@�x�@�p�@�`B@�X@�G�@�V@���@��H@�33@��w@��@��
@���@��@�|�@�\)@��\@��#@�X@��j@�I�@�t�@�ȴ@��!@�n�@��@���@���@�@��7@�hs@���@�p�@�X@�/@��u@�9X@���@�dZ@�@��H@���@���@���@��R@�^5@�@��^@�G�@�%@��@�9X@��m@�|�@�+@��y@��\@��@��T@��^@�O�@��/@��u@�I�@��@�b@���@��@��y@�^5@�$�@���@��7@�?}@���@�z�@�  @�t�@�K�@�"�@���@�-@�@��7@��@���@���@��@�bN@��@���@�\)@��@���@��@��\@�^5@�=q@�{@��#@��-@���@�`B@�&�@�Ĝ@�b@��w@�l�@�33@�o@��@��!@���@�$�@��@�?}@��@��@���@��u@���@���@�Z@�j@�1'@�1@�  @��m@���@�;d@�
=@�
=@��@�=q@��T@�@���@�7L@��@�Ĝ@��@��D@�Q�@���@��@�S�@��@���@�-@�5?@�K�@|�@s�@l9X@b=q@Y��@R=q@G�w@Ax�@:=q@2��@-O�@'�w@"��@�R@�H@ȴ@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B+B?}By�B�JB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�?BB�TB2-B_;B�B��B�B�-B�LB�jBŢBǮBȴBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��BȴBƨBÖB�XB�XB�jBǮBǮBĜB��B�jB�jB�jB�qB�'B�B��B��B��B��B��B��B�DB�PB�B|�Bn�B_;BXBI�BF�BF�BA�B/B�B	7B��B�mB��B��B��B�Bk�BYB49BB
��B
B
�LB
�B
��B
�B
o�B
jB
K�B
5?B
�B
+B	��B	��B	�B	�ZB	�5B	�B	��B	ĜB	�3B	��B	�\B	�PB	�B	� B	x�B	dZB	W
B	O�B	K�B	C�B	=qB	:^B	9XB	9XB	0!B	)�B	 �B	�B	1B�B�HB��B��B��B��B��BǮBɺBƨBB�RB�?B�B��B��B��B��B��B��B��B��B��B�uB�hB�7B�DB�DB�=B�7B�+B�1B�=B�PB�JB�JB�bB�hB�uB�JB�DB�7B�%B�B�B� B�B|�By�Bx�Bw�Bu�Bv�Bt�Bl�Bk�BjBjB]/BVBVBVBS�BQ�BP�BN�BT�BQ�BR�BT�BR�BQ�BVBP�BVBL�BR�B[#B@�B<jB:^BA�B=qB8RB;dBE�BD�B@�B@�BH�BL�BN�BR�BQ�BZBbNBhsBt�By�B� B�B�+B�hB��B��B��B��B��B�B�3B�XB�qB�}B�}B��BBÖBƨB��B��B��B��B�B�
B�B�B�
B�#B�#B�)B�/B�;B�`B�B�B��B��B��B��B	\B	�B	&�B	.B	/B	49B	6FB	8RB	:^B	<jB	>wB	B�B	A�B	@�B	D�B	E�B	B�B	?}B	A�B	H�B	M�B	O�B	Q�B	Q�B	R�B	VB	W
B	^5B	`BB	aHB	bNB	cTB	cTB	ffB	iyB	k�B	r�B	w�B	y�B	|�B	}�B	�%B	�B	�%B	�DB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�!B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�9B	�LB	�LB	�RB	�RB	�jB	�qB	�wB	�}B	�}B	��B	��B	��B	��B	��B	��B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�)B	�/B	�)B	�B	�)B	�
B	�
B	�
B	�
B	�B	�B	�B	�#B	�#B	�/B	�/B	�)B	�/B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�;B	�BB	�HB	�HB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�`B	�mB	�sB	�sB	�sB	�sB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B
B
PB
�B
�B
!�B
+B
2-B
9XB
C�B
I�B
Q�B
T�B
ZB
aHB
ffB
iyB
n�B
p�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
��B+B@�Bz�B�JB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�?B��B�TB33B`BB�%B��B�B�-B�LB�jBƨBȴBȴBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��BƨBǮBÖB��B��B��BɺBŢB��BB�}B��B�-B�!B�B��B��B��B��B��B�hB�\B�+B~�Br�BbNB\)BL�BH�BJ�BF�B33B�BJB��B�B�)B�B��B�7Bt�B`BBB�BhB
�B
ŢB
�^B
�-B
��B
�\B
s�B
u�B
VB
>wB
&�B
PB
B	��B	��B	�sB	�HB	�/B	��B	��B	��B	��B	�hB	�bB	�1B	�B	~�B	n�B	[#B	Q�B	O�B	G�B	?}B	;dB	;dB	;dB	49B	0!B	$�B	 �B	uB��B�B�B��B��B��B��B��B��B��BȴBB�dB�B��B��B��B��B��B��B��B��B��B��B��B�VB�JB�JB�DB�=B�DB�VB�VB�VB�VB�VB�oB��B��B�VB�PB�DB�JB�1B�+B�B�+B�B� B}�Bz�Bw�B{�By�Bn�Bm�Bm�Br�BaHBW
BXBYBVBT�BT�BR�BXBR�BT�BYBT�BW
BYBT�BYBP�BW
B^5BF�B@�B=qBC�B>wB8RB=qBH�BG�BB�BB�BI�BM�BO�BS�BS�B[#BbNBiyBu�B{�B�B� B�%B�hB��B��B��B��B��B�B�3B�^B�wB��B��BBÖBŢBȴB��B��B��B��B�B�B�B�#B�
B�)B�#B�/B�/B�BB�mB�B�B��B��B��B��B	VB	�B	&�B	/B	0!B	5?B	8RB	9XB	;dB	<jB	?}B	D�B	C�B	A�B	E�B	H�B	E�B	A�B	A�B	G�B	M�B	O�B	Q�B	Q�B	S�B	W
B	XB	^5B	aHB	bNB	dZB	e`B	dZB	gmB	jB	k�B	r�B	w�B	y�B	|�B	~�B	�+B	�%B	�%B	�DB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�'B	�!B	�B	�'B	�'B	�'B	�-B	�3B	�3B	�9B	�LB	�RB	�RB	�RB	�jB	�qB	�wB	��B	��B	B	B	B	��B	��B	��B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�/B	�#B	�/B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�5B	�/B	�5B	�/B	�5B	�/B	�5B	�;B	�5B	�5B	�5B	�;B	�;B	�;B	�HB	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�TB	�ZB	�ZB	�`B	�sB	�yB	�yB	�sB	�sB	�B	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B
B
PB
�B
�B
!�B
+B
2-B
:^B
C�B
I�B
Q�B
T�B
[#B
bNB
ffB
iyB
n�B
p�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
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
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451592012011014515920120110145159  AO  ARGQ                                                                        20111130143701  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143701  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145159  IP                  G�O�G�O�G�O�                