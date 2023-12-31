CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:19Z UW 3.1 conversion   
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               eA   AO  20111130141443  20190522121826  1727_5046_101                   2C  D   APEX                            2143                            040306                          846 @Ԙ=+��1   @Ԙ=�� @6�&�x���c�E���1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B��B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;�fD<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Dsl�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B��B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;�fD<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Dsl�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AӁA�l�A�`BA�`BA�bNA�^5A�\)A�ZA�VA�=qA�=qA�5?A�-A�$�A�"�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�bA�S�A�9XA�VA���A���A�|�A��mA��A�=qA�`BA�K�A���A��A��yA�ȴA�1'A�$�A�A�&�A�{A��HA��PA��A�x�A�I�A�E�A���A�(�A��A��-A�`BA�bA���A� �A��A�I�A��mA��DA�/A�;dA�$�A�n�A�A�A�bA��+A���A���A��/A��PA�bA�|�A�%A�x�A��A��RA���A���A�?}A�hsA���A���A��uA�"�A���A�A�-A��+A��A���A�ƨA���A�bA�v�A�ƨA�$�A�l�A��A�O�A~��A|�AyhsAvv�Au�As��Ap�\Aol�Ak\)AjAiXAhĜAe��Ab�`A`��A_�-A[`BAX�AV��ATbNAQ��APZAO�-AN�ANQ�AN1AMO�AL�AJv�AIAI`BAI�AI�AH�/AG?}AEhsAEAD~�AB��AA��AAdZA@ȴA@VA>��A=��A<�`A<$�A;�A;VA:��A:A9�A8�+A6E�A5G�A4�RA3��A3+A1t�A/��A.�A.r�A-��A-XA,^5A*�A)ƨA(�+A'�;A'G�A&��A%��A%/A%VA$��A#��A!t�A VA�7AbNA��A�A;dAz�A  A7LAp�AZA��A��A^5AA �A��A��A��A7LA��A�\A9XAt�AffA
�`A
ffA	��A	S�An�A  A7LA��A�wA
=A�jAn�A�-A�A(�A\)@��m@���@��j@�t�@�G�@�;d@�bN@�~�@�v�@���@��@� �@�V@⟾@�~�@�Q�@�l�@�J@أ�@�@�$�@�hs@���@�(�@ӝ�@�C�@�
=@ѡ�@϶F@�"�@Χ�@�v�@�J@ͺ^@�%@�I�@��;@˶F@˅@�v�@ɲ-@�G�@��`@ȋD@��
@ư!@�p�@î@�
=@�`B@��@�v�@�ff@��@���@�r�@�9X@�ƨ@�ȴ@�^5@��^@�V@���@�1@�33@�
=@���@�$�@��@��@�O�@���@�1@���@�;d@�ff@��^@�p�@���@�ff@���@�hs@���@�I�@��@�l�@�K�@��H@�5?@��@��-@�7L@�?}@�?}@�&�@�&�@��@�&�@�/@�/@�?}@�V@���@��`@��`@���@�I�@��F@�K�@��H@�v�@��T@��@��@�(�@�|�@�+@�E�@�E�@���@�^5@��H@��@��@�~�@��@��^@��@�@���@�@��@��@�=q@��-@�x�@��@� �@�33@�@���@�M�@���@�x�@�hs@�X@�O�@�G�@�G�@�X@�X@�hs@��7@�x�@�`B@�G�@�?}@�j@��P@��F@��@���@���@��D@�I�@�|�@�;d@��@�V@�5?@��@�@��@�@���@�hs@���@�  @��w@�t�@�|�@��P@���@���@�~�@�{@�&�@�t�@���@�-@�r�@�  @��F@�C�@�@�ȴ@��@��@���@���@��#@��@�@�@��@��-@��7@�X@��h@�p�@�?}@���@��/@��j@��u@�r�@�bN@�j@�Q�@�j@�j@�Q�@�Q�@�Q�@�I�@�9X@�  @���@���@�C�@�33@�o@��!@��+@�=q@���@��@�X@�/@�%@���@��j@���@��D@�r�@�I�@�9X@�(�@�b@�@�w111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AӁA�l�A�`BA�`BA�bNA�^5A�\)A�ZA�VA�=qA�=qA�5?A�-A�$�A�"�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�bA�S�A�9XA�VA���A���A�|�A��mA��A�=qA�`BA�K�A���A��A��yA�ȴA�1'A�$�A�A�&�A�{A��HA��PA��A�x�A�I�A�E�A���A�(�A��A��-A�`BA�bA���A� �A��A�I�A��mA��DA�/A�;dA�$�A�n�A�A�A�bA��+A���A���A��/A��PA�bA�|�A�%A�x�A��A��RA���A���A�?}A�hsA���A���A��uA�"�A���A�A�-A��+A��A���A�ƨA���A�bA�v�A�ƨA�$�A�l�A��A�O�A~��A|�AyhsAvv�Au�As��Ap�\Aol�Ak\)AjAiXAhĜAe��Ab�`A`��A_�-A[`BAX�AV��ATbNAQ��APZAO�-AN�ANQ�AN1AMO�AL�AJv�AIAI`BAI�AI�AH�/AG?}AEhsAEAD~�AB��AA��AAdZA@ȴA@VA>��A=��A<�`A<$�A;�A;VA:��A:A9�A8�+A6E�A5G�A4�RA3��A3+A1t�A/��A.�A.r�A-��A-XA,^5A*�A)ƨA(�+A'�;A'G�A&��A%��A%/A%VA$��A#��A!t�A VA�7AbNA��A�A;dAz�A  A7LAp�AZA��A��A^5AA �A��A��A��A7LA��A�\A9XAt�AffA
�`A
ffA	��A	S�An�A  A7LA��A�wA
=A�jAn�A�-A�A(�A\)@��m@���@��j@�t�@�G�@�;d@�bN@�~�@�v�@���@��@� �@�V@⟾@�~�@�Q�@�l�@�J@أ�@�@�$�@�hs@���@�(�@ӝ�@�C�@�
=@ѡ�@϶F@�"�@Χ�@�v�@�J@ͺ^@�%@�I�@��;@˶F@˅@�v�@ɲ-@�G�@��`@ȋD@��
@ư!@�p�@î@�
=@�`B@��@�v�@�ff@��@���@�r�@�9X@�ƨ@�ȴ@�^5@��^@�V@���@�1@�33@�
=@���@�$�@��@��@�O�@���@�1@���@�;d@�ff@��^@�p�@���@�ff@���@�hs@���@�I�@��@�l�@�K�@��H@�5?@��@��-@�7L@�?}@�?}@�&�@�&�@��@�&�@�/@�/@�?}@�V@���@��`@��`@���@�I�@��F@�K�@��H@�v�@��T@��@��@�(�@�|�@�+@�E�@�E�@���@�^5@��H@��@��@�~�@��@��^@��@�@���@�@��@��@�=q@��-@�x�@��@� �@�33@�@���@�M�@���@�x�@�hs@�X@�O�@�G�@�G�@�X@�X@�hs@��7@�x�@�`B@�G�@�?}@�j@��P@��F@��@���@���@��D@�I�@�|�@�;d@��@�V@�5?@��@�@��@�@���@�hs@���@�  @��w@�t�@�|�@��P@���@���@�~�@�{@�&�@�t�@���@�-@�r�@�  @��F@�C�@�@�ȴ@��@��@���@���@��#@��@�@�@��@��-@��7@�X@��h@�p�@�?}@���@��/@��j@��u@�r�@�bN@�j@�Q�@�j@�j@�Q�@�Q�@�Q�@�I�@�9X@�  @���@���@�C�@�33@�o@��!@��+@�=q@���@��@�X@�/@�%@���@��j@���@��D@�r�@�I�@�9X@�(�@�b@�@�w111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB@�B@�BB�BA�B@�BA�BA�BB�BB�BC�BC�BC�BC�BD�BD�BC�BC�BC�BC�BC�BD�BD�BD�BD�BD�BD�BD�BD�BD�BE�BE�BE�BE�BE�BE�BF�BF�BF�BI�Bq�B��B1B+B
=B+BB��B��B��B�B�B�yB�TB�
B��B��B��BBÖBB��B�^B�?B��B�bB�B�+B�uB��B��B��B�uB�JB�DB�Bt�BjB\)BR�BE�B:^B$�BJB%B��B�B�B��BÖB�qB�FB�B��B��B��B��B�DB�Bw�Bl�BffB_;BXB>wB1'B(�BbB
�HB
ɺB
�9B
��B
�uB
v�B
R�B
-B
0!B
7LB
#�B

=B	�sB	��B	ǮB	�jB	�B	�LB	��B	��B	��B	��B	�uB	t�B	[#B	M�B	-B	�B	hB	%B��B��B�B�B�B�B�yB�fB�TB�;B�5B�5B�;B�)B�
B�NB��B��B�B�B�B�B�B�fB�5B�B�B��B��B��B��B��B��BŢB��B�jB�RB�3B�B��B��B��B��B��B��B��B�bB�JB�=B�7B�7B�1B�+B�%B�B�B� B~�B|�B{�By�Bw�Bu�Bs�Br�Bo�Bo�Bo�Bm�BjBhsBffBffBffBffBe`BdZBdZBdZBbNBaHB^5B^5B]/B\)B[#BZBYBW
BVBT�BT�BT�BS�BR�BP�BM�BL�BI�BF�BE�BC�BA�BD�BC�BD�B:^B49B33B1'B2-B33B33B49B49B49B49B5?B5?B5?B6FB6FB7LB7LB7LB:^B?}B?}B?}B?}B?}B?}B?}B?}B?}B@�BA�BC�BB�BA�BB�BD�BF�BH�BJ�BM�BM�BN�BQ�BT�BT�BT�BW
BYBYBYB]/B^5B`BBbNBcTBe`BhsBjBm�Bm�Bs�By�B|�B�B�B�B�B�B�%B�B�B�B�%B�=B�oB��B��B��B��B��B�B�B�'B�FB�^B�dB�qB�}BÖBĜBŢBƨBǮBɺB��B��B��B��B��B��B��B��B��B�
B�#B�5B�BB�BB�;B�NB�sB�B�B��B��B��B��B��B��B��B��B	1B	JB	hB	�B	�B	�B	�B	�B	$�B	,B	.B	0!B	33B	6FB	6FB	6FB	6FB	6FB	7LB	8RB	;dB	;dB	<jB	>wB	@�B	A�B	A�B	@�B	@�B	B�B	C�B	C�B	E�B	F�B	F�B	E�B	E�B	E�B	F�B	H�B	I�B	J�B	K�B	L�B	P�B	T�B	ZB	]/B	^5B	^5B	_;B	bNB	cTB	dZB	k�B	l�B	k�B	iyB	e`B	dZB	bNB	aHB	aHB	dZB	hsB	iyB	jB	o�B	q�B	r�B	s�B	s�B	x�B	}�B	}�B	�B	�B	�+B	�JB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�-B	�FB	�LB	�RB	�dB	�qB	�qB	��B	B	ĜB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B@�B@�BB�BA�B@�BA�BA�BB�BB�BC�BC�BC�BC�BD�BD�BC�BC�BC�BC�BC�BD�BD�BD�BD�BD�BD�BD�BD�BD�BE�BE�BE�BE�BE�BE�BF�BF�BF�BJ�Bt�B��B�BuBJB	7B
=BBB��B�B�B�B�B�B��B��B��BǮBĜBĜBȴBÖB�dB��B�{B�1B�1B�{B��B��B��B��B�PB�VB�1Bz�Bp�B`BBXBI�B@�B0!B\BDB  B�B�B�#BƨB��B�XB�B��B��B��B��B�bB�B|�Bn�BhsB`BB_;BA�B49B-B�B
�fB
��B
�RB
��B
��B
~�B
[#B
0!B
33B
<jB
)�B
oB	�B	��B	��B	ĜB	�'B	��B	��B	��B	��B	�B	��B	z�B	^5B	YB	5?B	 �B	�B	VB	  B��B��B�B�B�B�B�B�`B�BB�;B�5B�BB�BB�#B�HB��B��B��B�B�B�B�B�B�BB�#B�B��B��B��B�B��B��BȴBB�wB�^B�RB�!B�B��B��B��B��B��B��B�{B�VB�JB�DB�JB�7B�1B�+B�1B�+B�B�B� B~�B}�Bz�Bw�Bu�Bu�Bu�Br�Bq�Bo�Bo�Bk�BhsBgmBffBffBffBffBdZBe`BdZBdZBbNB`BB_;B^5B^5B[#B[#BYBYBW
BVBVBVBT�BS�BO�BP�BL�BH�BG�BF�BD�BH�BF�BJ�B@�B6FB6FB6FB7LB:^B7LB6FB7LB7LB7LB7LB7LB6FB7LB7LB8RB8RB:^B=qB@�B@�B@�B@�B@�BA�B@�B@�B?}BA�BC�BD�BC�BB�BC�BE�BH�BJ�BM�BN�BO�BP�BS�BT�BVBW
BXBZBZB[#B^5B_;BaHBbNBdZBffBhsBk�Bn�Bq�Bv�Bz�B}�B�B�B�B�%B�%B�+B�%B�7B�%B�+B�DB�uB��B��B��B��B��B�B�B�-B�FB�^B�dB�qB�}BÖBĜBŢBƨBǮBɺB��B��B��B��B��B��B��B��B�B�B�)B�BB�HB�HB�HB�NB�yB�B�B��B��B��B��B��B��B��B��B	1B	JB	hB	�B	�B	�B	�B	�B	&�B	,B	.B	1'B	49B	6FB	6FB	6FB	6FB	6FB	7LB	8RB	;dB	;dB	<jB	>wB	@�B	A�B	A�B	B�B	B�B	B�B	C�B	D�B	H�B	H�B	G�B	G�B	F�B	F�B	G�B	H�B	I�B	J�B	K�B	L�B	P�B	T�B	[#B	^5B	^5B	^5B	_;B	bNB	cTB	e`B	l�B	m�B	l�B	k�B	ffB	e`B	e`B	bNB	bNB	e`B	hsB	iyB	k�B	o�B	q�B	r�B	s�B	s�B	x�B	}�B	}�B	�B	�B	�+B	�JB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�-B	�FB	�LB	�XB	�dB	�qB	�wB	��B	ÖB	ŢB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447092012010314470920120103144709  AO  ARGQ                                                                        20111130141443  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141443  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144709  IP                  G�O�G�O�G�O�                