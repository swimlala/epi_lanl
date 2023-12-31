CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:36Z UW 3.1 conversion   
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142811  20190522121827  1727_5046_160                   2C  D   APEX                            2143                            040306                          846 @��{gY@1   @��|W?�@6E�Q��c�V�u1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C33C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cy�fC{�fC~  C�  C�  C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Dy� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C33C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cy�fC{�fC~  C�  C�  C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� DifDi� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Dy� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA�VA�JA�JA�oA�oA�oA�VA�
=A�A��yA��;A��/A��#A���AϮAκ^Aˏ\A�C�A��A�I�A��AǬA�K�A��AƧ�AƃA�r�A�S�A�{AŲ-A�n�A���Aĺ^AđhA��A�p�A�1A¾wA�`BA�$�A��
A���A�5?A�G�A� �A��yA��RA��A�n�A�XA�bA��!A�+A�7LA�t�A�+A��jA�~�A�z�A�{A��yA�ĜA�"�A���A�7LA���A��jA�5?A�%A��A���A�A�A��9A�G�A�S�A�+A�A�A�33A�ZA��A��A�ƨA���A�S�A���A�~�A�(�A�"�A�\)A�=qA�M�A���A��mA��A��A��;A��A��7A�ĜA�9XA��A��HA��TA��A�JA���A�A��
A�&�A��A�`BA��A���A�`BA�=qA��A�A��A�\)A���A�JA�33A~n�A{\)Az �Ay��Ay�Ax�uAx  Aw7LAs��Aq��Ap�Ao��An�jAn~�An1Al��Al �Aj��Ai��AhffAfr�Ac�7AaC�A`jA\^5AYAW�FAT�AT�ASS�ARZAOdZAJ�9AHE�AE
=AA��A>��A9l�A6�HA5�hA4��A4z�A4E�A3��A1l�A/G�A.��A.�A,�A+�^A*�+A)�7A(ĜA'�#A'O�A'&�A&�RA&z�A&E�A&A%�A%oA$bA#K�A#
=A"�A"^5A!�mA ��A��A�9AE�A��AVA^5A
=A�FA�A?}A��A�uAbAE�A��AA�A7LAv�A�hA5?A�A/A��A
z�A	��AZA�PA��A��A;dA1A1'AG�A �!A �@�\)@�o@�=q@��@�K�@�"�@�t�@�;d@�~�@�@�x�@�z�@���@�h@�P@�R@�O�@�@� �@�Q�@��/@�9@��@���@��;@�dZ@�=q@�-@��@��m@�
=@�5?@��@�ȴ@ݡ�@�/@�@�{@���@Ѻ^@��@��`@�%@�X@��
@�~�@�-@��@�?}@�X@�O�@�Z@�7L@��@���@��@�+@�E�@�$�@��@š�@ŉ7@�G�@�A�@�;d@���@�z�@�dZ@�
=@��@��!@�~�@�~�@��@���@���@�@�p�@�V@��9@�r�@�C�@���@�%@���@��@�7L@�C�@�C�@���@���@�
=@�J@�G�@�bN@��@��;@�ƨ@���@���@��@��@��F@���@�K�@�
=@�ȴ@���@��+@�ff@�=q@���@��@��9@��w@�@���@�M�@�x�@���@��@���@��
@�r�@��@��@�@���@�p�@�?}@�7L@�&�@���@���@��@���@���@��@�E�@�n�@�@���@��h@�V@�A�@�;d@�E�@���@��D@�ƨ@��@�t�@�|�@�"�@��@�x�@��9@���@��@�V@��/@�(�@���@�@���@���@��h@��7@�X@���@���@�j@�I�@�b@���@�ƨ@��@�33@�
=@���@��y@��@��R@��\@�-@��#@�x�@�O�@�?}@��@�V@��`@��j@���@��D@�bN@���@��y@�~�@�^5@�ff@�M�@�=q@�-@��@���@�?}@�Ĝ@���@��D@�z�@�j@�bN@�Q�@�Z@�9X@��@�  @��m@��
@���@�l�@�S�@��y@���@���@��+@�^5@�M�@���@��-@��h@��7@��@�hs@�X@�7L@�Ĝ@�1'@�1@��m@��w@�|�@�dZ@�C�@�o@��!@�V@�5?@�@���@�p�@��@��u@�Q�@��;@��P@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�VA�VA�JA�JA�oA�oA�oA�VA�
=A�A��yA��;A��/A��#A���AϮAκ^Aˏ\A�C�A��A�I�A��AǬA�K�A��AƧ�AƃA�r�A�S�A�{AŲ-A�n�A���Aĺ^AđhA��A�p�A�1A¾wA�`BA�$�A��
A���A�5?A�G�A� �A��yA��RA��A�n�A�XA�bA��!A�+A�7LA�t�A�+A��jA�~�A�z�A�{A��yA�ĜA�"�A���A�7LA���A��jA�5?A�%A��A���A�A�A��9A�G�A�S�A�+A�A�A�33A�ZA��A��A�ƨA���A�S�A���A�~�A�(�A�"�A�\)A�=qA�M�A���A��mA��A��A��;A��A��7A�ĜA�9XA��A��HA��TA��A�JA���A�A��
A�&�A��A�`BA��A���A�`BA�=qA��A�A��A�\)A���A�JA�33A~n�A{\)Az �Ay��Ay�Ax�uAx  Aw7LAs��Aq��Ap�Ao��An�jAn~�An1Al��Al �Aj��Ai��AhffAfr�Ac�7AaC�A`jA\^5AYAW�FAT�AT�ASS�ARZAOdZAJ�9AHE�AE
=AA��A>��A9l�A6�HA5�hA4��A4z�A4E�A3��A1l�A/G�A.��A.�A,�A+�^A*�+A)�7A(ĜA'�#A'O�A'&�A&�RA&z�A&E�A&A%�A%oA$bA#K�A#
=A"�A"^5A!�mA ��A��A�9AE�A��AVA^5A
=A�FA�A?}A��A�uAbAE�A��AA�A7LAv�A�hA5?A�A/A��A
z�A	��AZA�PA��A��A;dA1A1'AG�A �!A �@�\)@�o@�=q@��@�K�@�"�@�t�@�;d@�~�@�@�x�@�z�@���@�h@�P@�R@�O�@�@� �@�Q�@��/@�9@��@���@��;@�dZ@�=q@�-@��@��m@�
=@�5?@��@�ȴ@ݡ�@�/@�@�{@���@Ѻ^@��@��`@�%@�X@��
@�~�@�-@��@�?}@�X@�O�@�Z@�7L@��@���@��@�+@�E�@�$�@��@š�@ŉ7@�G�@�A�@�;d@���@�z�@�dZ@�
=@��@��!@�~�@�~�@��@���@���@�@�p�@�V@��9@�r�@�C�@���@�%@���@��@�7L@�C�@�C�@���@���@�
=@�J@�G�@�bN@��@��;@�ƨ@���@���@��@��@��F@���@�K�@�
=@�ȴ@���@��+@�ff@�=q@���@��@��9@��w@�@���@�M�@�x�@���@��@���@��
@�r�@��@��@�@���@�p�@�?}@�7L@�&�@���@���@��@���@���@��@�E�@�n�@�@���@��h@�V@�A�@�;d@�E�@���@��D@�ƨ@��@�t�@�|�@�"�@��@�x�@��9@���@��@�V@��/@�(�@���@�@���@���@��h@��7@�X@���@���@�j@�I�@�b@���@�ƨ@��@�33@�
=@���@��y@��@��R@��\@�-@��#@�x�@�O�@�?}@��@�V@��`@��j@���@��D@�bN@���@��y@�~�@�^5@�ff@�M�@�=q@�-@��@���@�?}@�Ĝ@���@��D@�z�@�j@�bN@�Q�@�Z@�9X@��@�  @��m@��
@���@�l�@�S�@��y@���@���@��+@�^5@�M�@���@��-@��h@��7@��@�hs@�X@�7L@�Ĝ@�1'@�1@��m@��w@�|�@�dZ@�C�@�o@��!@�V@�5?@�@���@�p�@��@��u@�Q�@��;@��P@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB{�B|�B|�B|�B|�B|�B}�B}�B}�B|�B|�B{�B{�B{�B{�B{�B�=B��B�NB�fB�mB�B�B�B��B��B��BBBB%B	7BDBbB�B�B�B#�B%�B'�B&�B.B6FB7LB:^B>wB?}B?}BH�BJ�BK�BO�BT�BcTBy�B�%B�=B��B��B�oB��B�B�!B�!B�B�B�RB�XB��B��B� B~�B��B��B��B��B��B��B�hB�=BbNBN�BF�BA�B49B(�B�BDBB��B�HB��B�jB��B��B��B��B�%B[#BI�B8RB33B0!B.B,B(�B�B�BDB
��B
�B
�sB
�NB
�)B
�B
��B
��B
��B
ǮB
ŢB
B
�RB
�bB
C�B
"�B
VB
%B
B	��B	��B	��B	�B	�)B	��B	��B	ŢB	�wB	�dB	�FB	�B	��B	��B	�oB	�%B	s�B	^5B	I�B	@�B	&�B	�B	
=B��B��B�B�yB�
BĜB�dB�-B��B��B�hB�\B�VB�JB�JB�=B�1B�B�B�B~�B|�By�Bx�Bw�Bv�Bu�Bu�Bt�Bs�Bs�Br�Br�Bp�Bo�Bn�Bn�Bn�Bm�Bl�BjBhsBiyBhsBgmBffBe`BdZBe`BffBffBffBffBffBe`B`BB`BBaHB`BB_;BaHB^5BaHB^5B\)BaHBffBdZB`BB]/B\)B[#BYBVBYB^5BbNBdZBffBgmBhsBffBcTBffBjBl�Bm�Bn�Bl�BiyBffBbNBe`Be`BgmBo�Bw�B{�B�B�B�JB�\B�hB�\B�oB�uB�oB�\B�JB�PB��B��B��B�hB�PB�DB�B� B~�B~�B�B�B~�B{�B�B�{B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�FB�FB�FB�LB�XBǮB��B��B��B�B�
B�B�TB�B�B�B�B��B��B��B��B��B	B	B	B��B��B��B	  B	JB	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	"�B	"�B	"�B	"�B	%�B	(�B	)�B	2-B	5?B	6FB	6FB	6FB	6FB	7LB	;dB	>wB	A�B	A�B	A�B	C�B	F�B	I�B	J�B	J�B	J�B	K�B	K�B	K�B	L�B	Q�B	[#B	`BB	dZB	e`B	gmB	ffB	gmB	iyB	gmB	e`B	cTB	cTB	dZB	l�B	t�B	y�B	z�B	{�B	{�B	x�B	q�B	o�B	o�B	p�B	n�B	o�B	t�B	w�B	y�B	y�B	z�B	|�B	�B	�B	�+B	�1B	�=B	�JB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�!B	�!B	�!B	�!B	�'B	�-B	�9B	�FB	�LB	�LB	�LB	�LB	�RB	�XB	�dB	�}B	��B	��B	��B	��B	ÖB	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�N1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B{�B|�B|�B|�B|�B|�B}�B}�B}�B|�B|�B{�B{�B{�B|�B�B��B�B�B�yB�yB�B�B��B��B��B��BBBB+B
=BJBhB�B�B �B$�B'�B(�B.B49B9XB<jB@�B?}BA�BE�BJ�BK�BM�BR�BYBjB~�B�1B�PB��B��B�{B��B�B�3B�3B�B�B�XB�wB�B��B�B~�B��B��B��B��B��B��B��B��BjBO�BH�BH�B<jB1'B'�BPB
=BB�fB�#BÖB�B�B��B��B��BcTBVB<jB49B2-B.B.B.B"�B�B{B  B
��B
�B
�`B
�;B
�B
��B
��B
��B
ȴB
ǮB
ĜB
�wB
��B
G�B
&�B
bB
+B
B
  B	��B	��B	��B	�NB	��B	��B	ɺB	�}B	�wB	�dB	�3B	�B	��B	��B	�hB	�B	iyB	P�B	T�B	5?B	�B	�B	B��B��B�B�ZB��BB�LB�B��B��B�oB�bB�PB�PB�JB�bB�JB�%B�B�B�B~�B|�Bz�Bz�Bw�Bv�Bv�Bt�Bt�Bs�Bs�Bs�Bs�Bq�Bo�Bo�Bo�Bn�Bo�Bm�BjBjBiyBhsBhsBhsBiyBgmBgmBgmBgmBgmBhsBffBbNBbNBaHBaHBcTBbNBcTB`BBaHBdZBhsBhsBcTB`BB_;B]/B]/B\)B\)B`BBdZBe`BgmBhsBjBiyBiyBk�Bk�Bm�Bn�Bo�Bn�BjBjBe`BgmBgmBhsBp�Bw�Bz�B�B�1B�VB�hB�oB�hB�uB�{B�{B�hB�PB�JB��B��B��B��B�PB�bB�7B�B~�B~�B�B�+B�B{�B� B�oB��B��B�B��B�'B�-B��B�B��B��B��B��B��B��B��B�B�B�'B�!B�9B�FB�FB�FB�LB�^BǮB��B��B��B�
B�B�#B�`B�B�B�B�B��B��B��B��B��B	1B	B	B	  B��B��B	  B	JB	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	"�B	"�B	"�B	#�B	&�B	)�B	,B	33B	5?B	7LB	6FB	7LB	7LB	8RB	;dB	=qB	C�B	D�B	A�B	C�B	F�B	I�B	J�B	J�B	J�B	K�B	K�B	K�B	L�B	P�B	[#B	`BB	e`B	e`B	hsB	gmB	hsB	k�B	iyB	gmB	dZB	dZB	e`B	k�B	t�B	z�B	|�B	|�B	}�B	|�B	r�B	o�B	o�B	q�B	p�B	p�B	t�B	w�B	y�B	y�B	z�B	}�B	�B	�B	�+B	�1B	�=B	�JB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�'B	�!B	�'B	�!B	�!B	�!B	�!B	�-B	�3B	�?B	�FB	�LB	�LB	�LB	�LB	�RB	�XB	�dB	�}B	��B	��B	��B	B	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�N1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<���<#�
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
<49X<T��<49X<#�
<��
<e`B<#�
<49X<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447302012010314473020120103144730  AO  ARGQ                                                                        20111130142811  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142811  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144730  IP                  G�O�G�O�G�O�                