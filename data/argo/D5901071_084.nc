CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:14Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kl   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  UP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               TA   AO  20111130141045  20190522121826  1727_5046_084                   2C  D   APEX                            2143                            040306                          846 @Ԃ@V� 1   @ԂA��@8��v��d-1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|�C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D��D� D  D�fD  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DL��DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy��D��D�s3D���D��3D�,�D�s3D�i�D�� D�3D�S3D���D�� D�  D�c3Dڰ D��fD�)�D�i�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|�C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D��D� D  D�fD  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DL��DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy��D��D�s3D���D��3D�,�D�s3D�i�D�� D�3D�S3D���D�� D�  D�c3Dڰ D��fD�)�D�i�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A˸RA˲-A˰!A˟�A˓uA�z�A�^5A�\)A�E�A�1'A�+A� �A��A��A�{A�VA�%A�  A���A���A���A��A��A��A��mA��/AʑhA���A���A��Aě�A�VA��A��HA�\)A���A�r�A��A�G�A�;dA�VA�v�A�1'A��/A���A�hsA�bA���A�+A���A�-A���A�p�A�Q�A��HA�x�A�VA��mA�l�A���A�^5A��HA�C�A�K�A���A�p�A��A�ĜA�1'A��/A��+A�  A���A�jA��A�A�p�A��
A���A��yA�S�A�|�A�dZA�{A���A�t�A���A��9A��/A��
A��A�v�A��uA�+A�5?A���A���A���A��`A��-A��A�I�A���A��A���A�n�A�/A�A���A��A�C�A�A�E�A���A��A��jA��;A��;A�  A�bNA���A���A��A}
=A{hsAy�mAxZAu�hAt{Ar �Ap{An��An^5Al�uAi�Af5?AdbNAcC�A`ffA^��A^n�A]ƨA];dA\$�AZ�RAY7LAW�-AV�yAV�HAU�AU\)AU�AT�yAS�;ARv�AQ��AP�AO&�AMALbNAK�mAK�AKdZAJv�AI
=AG�AFE�AEoADE�AC�
AC��ACC�ABE�AAA@=qA?��A?p�A>�A>5?A=�A=�wA=��A=S�A<=qA;�A:JA9&�A8��A7�;A6��A6I�A5A5��A5XA3�;A2v�A0��A/%A-�
A,�DA+%A)�;A(�jA(ZA'�TA'XA&��A%oA$1'A#ƨA#�hA#VA"��A"1A!�mA!�A!�PA!x�A!S�A ��A�A�7A��A �A��A�AƨA��A��A�`Av�AK�A�AffAp�A"�AA��AȴA�A��AdZA��A��A��A^5AA
��A
A�A	XA�AAXA��A;dA��A$�A�hA��AȴA�;A @�V@��@���@���@��
@�v�@��@�Z@�\@�A�@�p�@�@��@��T@�bN@���@�+@�-@߶F@�`B@�l�@���@�7L@���@�9X@�M�@�V@�Q�@Ұ!@���@ѩ�@д9@ύP@�K�@���@��@���@̓u@��;@�@�ƨ@�S�@�"�@Ƈ+@�p�@�X@�/@ě�@Ý�@��y@�@���@��@��u@��F@���@�@�O�@��D@�;d@�?}@�Ĝ@�1@��@�=q@�7L@�Ĝ@�j@���@�K�@�=q@��@�9X@�1@���@���@���@�-@�x�@�/@���@���@�A�@��F@�33@��\@��@�&�@�b@��@��@�ff@���@�hs@�%@�r�@�Q�@���@�+@��R@�v�@�^5@���@�?}@���@��D@��D@���@���@���@���@�V@�O�@��^@���@���@��j@��j@���@��D@���@���@�V@�$�@�J@��7@��7@��h@��h@��h@�x�@�7L@�/@�/@�j@��
@���@���@�1'@���@��!@�^5@�-@���@��#@�V@�+@�E�@�Q�@�A�@�K�@�ff@�{@���@�p�@��@��@��@��`@��@�O�@�&�@��@�Ĝ@���@�|�@�X@��`@���@��`@�Ĝ@�j@�Ĝ@���@�9X@��m@��@�{@�hs@�V@���@���@���@�Z@���@��@���@���@��@��-@���@��@�G�@�p�@���@���@�o@�n�@�v�@���@���@��w@��P@�ƨ@�9X@�/@��@��@��`@��@��
@��
@�ƨ@���@�o@�v�@�=q@�$�@��@��7@�O�@�G�@�G�@�"�@}@v��@kS�@`bN@Y��@T(�@I�#@B�H@<9X@6ff@.ff@'��@!�@�@1'@�@l�@�@
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A˸RA˲-A˰!A˟�A˓uA�z�A�^5A�\)A�E�A�1'A�+A� �A��A��A�{A�VA�%A�  A���A���A���A��A��A��A��mA��/AʑhA���A���A��Aě�A�VA��A��HA�\)A���A�r�A��A�G�A�;dA�VA�v�A�1'A��/A���A�hsA�bA���A�+A���A�-A���A�p�A�Q�A��HA�x�A�VA��mA�l�A���A�^5A��HA�C�A�K�A���A�p�A��A�ĜA�1'A��/A��+A�  A���A�jA��A�A�p�A��
A���A��yA�S�A�|�A�dZA�{A���A�t�A���A��9A��/A��
A��A�v�A��uA�+A�5?A���A���A���A��`A��-A��A�I�A���A��A���A�n�A�/A�A���A��A�C�A�A�E�A���A��A��jA��;A��;A�  A�bNA���A���A��A}
=A{hsAy�mAxZAu�hAt{Ar �Ap{An��An^5Al�uAi�Af5?AdbNAcC�A`ffA^��A^n�A]ƨA];dA\$�AZ�RAY7LAW�-AV�yAV�HAU�AU\)AU�AT�yAS�;ARv�AQ��AP�AO&�AMALbNAK�mAK�AKdZAJv�AI
=AG�AFE�AEoADE�AC�
AC��ACC�ABE�AAA@=qA?��A?p�A>�A>5?A=�A=�wA=��A=S�A<=qA;�A:JA9&�A8��A7�;A6��A6I�A5A5��A5XA3�;A2v�A0��A/%A-�
A,�DA+%A)�;A(�jA(ZA'�TA'XA&��A%oA$1'A#ƨA#�hA#VA"��A"1A!�mA!�A!�PA!x�A!S�A ��A�A�7A��A �A��A�AƨA��A��A�`Av�AK�A�AffAp�A"�AA��AȴA�A��AdZA��A��A��A^5AA
��A
A�A	XA�AAXA��A;dA��A$�A�hA��AȴA�;A @�V@��@���@���@��
@�v�@��@�Z@�\@�A�@�p�@�@��@��T@�bN@���@�+@�-@߶F@�`B@�l�@���@�7L@���@�9X@�M�@�V@�Q�@Ұ!@���@ѩ�@д9@ύP@�K�@���@��@���@̓u@��;@�@�ƨ@�S�@�"�@Ƈ+@�p�@�X@�/@ě�@Ý�@��y@�@���@��@��u@��F@���@�@�O�@��D@�;d@�?}@�Ĝ@�1@��@�=q@�7L@�Ĝ@�j@���@�K�@�=q@��@�9X@�1@���@���@���@�-@�x�@�/@���@���@�A�@��F@�33@��\@��@�&�@�b@��@��@�ff@���@�hs@�%@�r�@�Q�@���@�+@��R@�v�@�^5@���@�?}@���@��D@��D@���@���@���@���@�V@�O�@��^@���@���@��j@��j@���@��D@���@���@�V@�$�@�J@��7@��7@��h@��h@��h@�x�@�7L@�/@�/@�j@��
@���@���@�1'@���@��!@�^5@�-@���@��#@�V@�+@�E�@�Q�@�A�@�K�@�ff@�{@���@�p�@��@��@��@��`@��@�O�@�&�@��@�Ĝ@���@�|�@�X@��`@���@��`@�Ĝ@�j@�Ĝ@���@�9X@��m@��@�{@�hs@�V@���@���@���@�Z@���@��@���@���@��@��-@���@��@�G�@�p�@���@���@�o@�n�@�v�@���@���@��w@��P@�ƨ@�9X@�/@��@��@��`@��@��
@��
@�ƨ@���@�o@�v�@�=q@�$�@��@��7@�O�@�G�@�G�@�"�@}@v��@kS�@`bN@Y��@T(�@I�#@B�H@<9X@6ff@.ff@'��@!�@�@1'@�@l�@�@
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B!�B �B!�B"�B"�B �B �B!�B �B �B �B!�B!�B"�B"�B#�B#�B#�B$�B$�B$�B%�B%�B%�B"�B�B��B�BǮB��B��BƨBÖBÖB��B��B�yB�B�B��B1BVB�B�B!�B+B9XBE�BP�BcTBt�Bw�Bz�B�B�\B�hB��B��B�?B�RB�}B�}B�dB�jB�jB�jB�jBŢB��B��BǮBŢBÖB��B�wB�^B�?B��B��B��B��B��B��B�bBw�Be`BZBF�B;dB2-B)�B!�B�BVBB��B�B�HB�)B��B�9B��B�7B{�Bu�Bp�BffBJ�B8RB&�BVB  B
�NB
ȴB
�B
��B
�VB
~�B
r�B
ffB
J�B
.B
�B
PB
  B	��B	�B	�B	�HB	�B	�B	�B	��B	�jB	�-B	��B	��B	��B	�{B	�hB	�PB	�1B	�B	y�B	u�B	p�B	o�B	�B	�B	�B	�B	�B	{�B	u�B	q�B	k�B	e`B	]/B	YB	XB	VB	YB	YB	P�B	H�B	@�B	:^B	7LB	6FB	5?B	2-B	,B	'�B	$�B	"�B	�B	�B	�B	�B	�B	{B	hB	DB	%B	%B	B	  B��B��B�B�B�B�yB�NB�#B��B��BɺBÖB�qB�XB�LB�?B�3B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�\B�JB�7B�%B�B�B}�B|�Bz�Bx�Bw�Bw�Bw�Bw�Bu�Bq�Bp�Bo�Bm�BhsBdZBaHB_;B]/B\)BZB\)B\)B[#BYBXBVBS�BR�BP�BN�BJ�BE�BC�BA�B?}B>wB>wB=qB<jB:^B8RB9XB;dB9XB8RB7LB9XB9XB9XB7LB8RB8RB8RB8RB8RB7LB6FB6FB7LB6FB8RB8RB7LB8RB9XB8RB8RB9XB:^B:^B9XB:^B>wB>wB>wB>wB@�B@�B?}B?}B?}B@�B@�BA�BC�BD�BE�BF�BF�BG�BG�BH�BM�BM�BO�BQ�BR�BT�BVBVBXBXBZB`BBbNBdZBffBk�Bl�Bm�Bq�Br�Br�Bs�Bt�Bv�Bx�Bz�B|�B� B�B�+B�=B�JB�bB�uB��B��B��B��B��B��B�B�B�'B�3B�LB�jB��BɺB��B��B�B�B�BB�mB�B�B�B�B�B�B�B�B�B�B��B	B	B	B	B	%B		7B	JB	JB	JB	JB	PB	JB	B��B��B��B��B��B��B��B	B		7B	%B	B	B	%B	+B		7B	
=B	VB	{B	!�B	33B	:^B	=qB	>wB	@�B	C�B	B�B	@�B	>wB	;dB	=qB	D�B	G�B	I�B	L�B	O�B	Q�B	T�B	VB	W
B	S�B	Q�B	R�B	R�B	T�B	T�B	XB	`BB	ffB	k�B	s�B	�B	�=B	�1B	�JB	��B	��B	��B	��B	��B	��B	��B	�oB	�hB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	B	�5B	�B	��B
1B
�B
!�B
#�B
-B
5?B
=qB
F�B
J�B
N�B
XB
]/B
dZB
jB
o�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B!�B �B!�B"�B"�B �B �B!�B �B �B �B!�B!�B"�B"�B#�B#�B#�B$�B$�B$�B%�B%�B%�B$�B#�B1B�HBɺB��B��BǮBŢBŢB��B�
B�B�B��B��B	7B\B�B�B#�B-B;dBG�BS�BffBu�Bx�B|�B�+B�bB�uB��B��B�FB�dBBĜB�wB�qB�wB�wB�}BƨB��B��BȴBƨBǮBB��B��B�dB�'B��B�B�B��B��B��B}�BiyBaHBJ�B?}B8RB.B#�B�BhB	7B��B�B�NB�;B�#B�jB��B�PB}�Bv�Bs�Bo�BP�B<jB.BuB
=B
�yB
��B
�3B
��B
�uB
�B
x�B
r�B
VB
7LB
�B
hB
B	��B	��B	�B	�fB	�)B	�#B	�/B	�B	ÖB	�LB	�B	��B	��B	��B	�uB	�\B	�DB	�%B	~�B	z�B	r�B	o�B	�B	�B	�B	�B	�B	~�B	v�B	s�B	o�B	hsB	`BB	ZB	YB	VB	[#B	]/B	T�B	L�B	C�B	<jB	8RB	7LB	6FB	5?B	0!B	)�B	%�B	#�B	!�B	�B	�B	�B	�B	�B	{B	VB		7B		7B	B	B��B��B�B�B�B�B�fB�BB�B��B��BȴB��B�jB�RB�LB�?B�9B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�bB�PB�=B�%B�B�B~�B|�B{�Bx�Bx�Bw�Bx�By�Bu�Br�Bq�Bp�Bn�BhsBcTBbNB^5B^5B[#B^5B^5B]/B]/BYBXBVBT�BQ�BQ�BO�BI�BG�BF�BB�B@�B@�B?}B>wB=qB<jB=qB=qB;dB;dB:^B:^B;dB;dB;dB<jB8RB;dB:^B9XB9XB:^B9XB9XB9XB:^B8RB9XB:^B9XB9XB:^B:^B;dB;dB<jB:^B?}B>wB?}B@�B@�B@�B@�BA�B@�BA�BA�BB�BD�BE�BG�BG�BG�BH�BI�BK�BN�BN�BQ�BR�BT�BVBW
BW
BYBZB]/BaHBcTBe`BffBk�Bm�Bn�Br�Bs�Bs�Bt�Bu�Bw�By�B{�B~�B�B�%B�1B�=B�JB�hB�{B��B��B��B��B��B��B�B�B�-B�9B�LB�jB��BɺB��B��B�B�B�BB�mB�B�B�B�B��B�B�B�B�B�B��B	B	B	B	B	%B	
=B	JB	JB	VB	PB	VB	\B		7B	B��B��B��B��B��B��B	B	DB	
=B	B	+B	1B	1B	
=B	DB	PB	hB	�B	33B	9XB	=qB	>wB	@�B	D�B	D�B	A�B	B�B	<jB	<jB	E�B	G�B	J�B	L�B	O�B	R�B	VB	W
B	ZB	T�B	R�B	R�B	R�B	T�B	VB	XB	_;B	e`B	jB	p�B	�B	�DB	�7B	�=B	��B	��B	��B	��B	��B	��B	��B	�uB	�uB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	B	�5B	�B	��B
1B
�B
!�B
#�B
-B
5?B
=qB
F�B
J�B
N�B
XB
^5B
dZB
jB
o�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447032012010314470320120103144703  AO  ARGQ                                                                        20111130141045  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141045  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144703  IP                  G�O�G�O�G�O�                