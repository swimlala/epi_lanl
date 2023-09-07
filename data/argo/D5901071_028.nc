CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:59Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130135707  20190522121825  1727_5046_028                   2C  D   APEX                            2143                            040306                          846 @�9�2��1   @�9�ff�@7l������c���$�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\�fD]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dyl�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK�fDL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\�fD]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dyl�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AН�AУ�AХ�AЧ�AЧ�AЩ�AЮAа!Aд9Aд9Aд9Aд9Aв-AЮAС�AН�AЗ�AЗ�AЙ�AЛ�AЛ�AН�AС�AП�A��A�z�A� �Aħ�A���AĬA��A�S�A�  A�O�A��A�A��A�E�A��A�=qA��RA���A��PA�G�A��A��uA��+A��A��A�K�A�A�`BA��yA���A�r�A�^5A�7LA��wA��A�l�A�{A�ĜA�hsA��A�A��PA��A�ȴA���A��uA��A�x�A�`BA�E�A�A�A���A�A�A���A��A�$�A��A� �A�ƨA�v�A�7LA��;A���A��A��uA�(�A��A��A��A��wA�S�A���A��A���A��+A�
=A�|�A��A��RA��wA��
A�XA�%A���A�K�A���A��DA�bNA�|�A�
=A���A��FA�&�A���A�ȴA��+A�O�A��TA���A���A�G�A�\)A���A�=qA��^A��`A���A�^5A���A�\)A�"�A��A�=qA���A��A� �A�dZA~$�AyK�AuXAt�AsVAp��AoAo��Ao�AnZAl��Aj�uAg�Ae�;Aa�A]`BA[;dAY��AX�uAV��AU`BAS�-ARz�AQ�PAM�ALr�AKVAI�#AIƨAI\)AH��AG�AF�HAF1'AEVAD9XAC�hAAXA=|�A<~�A:��A8^5A7�mA7O�A4E�A3C�A3"�A2�9A1��A1+A/�FA/G�A.�HA.�A-��A+��A*Q�A)&�A'�
A'dZA'A&I�A%��A$��A#��A#G�A"��A!��A�A�Av�AZA��A�AO�A�\AE�A&�AVA��AQ�AK�AjA+AjA�hAn�AAr�A�mA�FA\)A��A��A�-AO�A%A
�A
1A��A&�A�jA�AS�A��AjAA�A-A1A�A~�A��AG�A�A �@�E�@���@��@���@��u@��h@�@��@�G�@�F@�@�V@�E�@�V@�+@�$�@��
@�@�  @◍@�{@�V@�V@��u@�
=@�ȴ@���@�{@ݲ-@�b@��@�9@�-@���@�l�@�n�@�hs@���@�9X@�+@��@ف@؋D@���@�l�@ա�@�Z@�1@�l�@�%@�ff@�/@̬@�r�@�
=@�-@�$�@ȴ9@�{@�p�@ă@�@�-@��-@���@� �@���@���@���@�G�@���@��@�@��^@���@�z�@�1@���@��!@�{@���@��9@�ƨ@��@��R@��\@��\@���@�n�@�hs@�I�@�ƨ@�t�@�@���@��!@�@�r�@��@�5?@��T@��@�?}@��u@�b@�
=@�E�@���@�V@�A�@��@�$�@���@���@��@�X@�V@���@�dZ@��@�ȴ@���@�$�@�x�@�Ĝ@��@��@���@�=q@�=q@�-@�$�@�J@��@���@���@��@���@��D@�1'@�t�@�@���@��\@�5?@�@��@�%@���@�1'@��F@�|�@�S�@�K�@�;d@�+@���@�M�@�5?@���@��@�%@�Q�@��F@�S�@�"�@��y@���@�n�@�E�@�{@���@�p�@�`B@�p�@���@�p�@�/@���@���@��j@�Ĝ@��j@��D@�z�@�z�@�j@�1'@�1@���@��w@��@�t�@�+@��y@��R@���@���@���@���@���@�~�@�ff@�E�@�{@��T@�hs@��@�I�@���@�t�@�l�@�+@��@��@��H@�~�@�$�@�J@���@��#@���@�@�G�@���@���@��9@���@��D@�{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AН�AУ�AХ�AЧ�AЧ�AЩ�AЮAа!Aд9Aд9Aд9Aд9Aв-AЮAС�AН�AЗ�AЗ�AЙ�AЛ�AЛ�AН�AС�AП�A��A�z�A� �Aħ�A���AĬA��A�S�A�  A�O�A��A�A��A�E�A��A�=qA��RA���A��PA�G�A��A��uA��+A��A��A�K�A�A�`BA��yA���A�r�A�^5A�7LA��wA��A�l�A�{A�ĜA�hsA��A�A��PA��A�ȴA���A��uA��A�x�A�`BA�E�A�A�A���A�A�A���A��A�$�A��A� �A�ƨA�v�A�7LA��;A���A��A��uA�(�A��A��A��A��wA�S�A���A��A���A��+A�
=A�|�A��A��RA��wA��
A�XA�%A���A�K�A���A��DA�bNA�|�A�
=A���A��FA�&�A���A�ȴA��+A�O�A��TA���A���A�G�A�\)A���A�=qA��^A��`A���A�^5A���A�\)A�"�A��A�=qA���A��A� �A�dZA~$�AyK�AuXAt�AsVAp��AoAo��Ao�AnZAl��Aj�uAg�Ae�;Aa�A]`BA[;dAY��AX�uAV��AU`BAS�-ARz�AQ�PAM�ALr�AKVAI�#AIƨAI\)AH��AG�AF�HAF1'AEVAD9XAC�hAAXA=|�A<~�A:��A8^5A7�mA7O�A4E�A3C�A3"�A2�9A1��A1+A/�FA/G�A.�HA.�A-��A+��A*Q�A)&�A'�
A'dZA'A&I�A%��A$��A#��A#G�A"��A!��A�A�Av�AZA��A�AO�A�\AE�A&�AVA��AQ�AK�AjA+AjA�hAn�AAr�A�mA�FA\)A��A��A�-AO�A%A
�A
1A��A&�A�jA�AS�A��AjAA�A-A1A�A~�A��AG�A�A �@�E�@���@��@���@��u@��h@�@��@�G�@�F@�@�V@�E�@�V@�+@�$�@��
@�@�  @◍@�{@�V@�V@��u@�
=@�ȴ@���@�{@ݲ-@�b@��@�9@�-@���@�l�@�n�@�hs@���@�9X@�+@��@ف@؋D@���@�l�@ա�@�Z@�1@�l�@�%@�ff@�/@̬@�r�@�
=@�-@�$�@ȴ9@�{@�p�@ă@�@�-@��-@���@� �@���@���@���@�G�@���@��@�@��^@���@�z�@�1@���@��!@�{@���@��9@�ƨ@��@��R@��\@��\@���@�n�@�hs@�I�@�ƨ@�t�@�@���@��!@�@�r�@��@�5?@��T@��@�?}@��u@�b@�
=@�E�@���@�V@�A�@��@�$�@���@���@��@�X@�V@���@�dZ@��@�ȴ@���@�$�@�x�@�Ĝ@��@��@���@�=q@�=q@�-@�$�@�J@��@���@���@��@���@��D@�1'@�t�@�@���@��\@�5?@�@��@�%@���@�1'@��F@�|�@�S�@�K�@�;d@�+@���@�M�@�5?@���@��@�%@�Q�@��F@�S�@�"�@��y@���@�n�@�E�@�{@���@�p�@�`B@�p�@���@�p�@�/@���@���@��j@�Ĝ@��j@��D@�z�@�z�@�j@�1'@�1@���@��w@��@�t�@�+@��y@��R@���@���@���@���@���@�~�@�ff@�E�@�{@��T@�hs@��@�I�@���@�t�@�l�@�+@��@��@��H@�~�@�$�@�J@���@��#@���@�@�G�@���@���@��9@���@��D@�{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB>wB>wB>wB>wB>wB>wB>wB?}B?}B>wB>wB>wB>wB@�BB�BB�BD�BD�BE�BE�BF�BF�BF�BD�B49B=qBQ�Bv�Bw�Bv�B�B�%B�1B�PB��B��B��B��B��B�!BĜBǮB�B�#B�/B�fB��B%BPB\BoB�B�B"�B$�B$�B%�B&�B'�B(�B)�B+B+B+B)�B/B0!B1'B33B49B6FB7LB8RB9XB<jB<jB9XB49B0!B-B)�B$�B�B�BuB\BDB	7BB��B��B�B�TB�HB�5B�B��B��BĜB�jB�-B��B��B�=Bx�BiyB`BBYBO�BI�BB�B8RB"�BbB%B  B��B�B�}B�B��B��B�{B�%B}�Bu�BcTBJ�B@�B6FB%�BhB
��B
�B
�
B
��B
��B
�{B
�B
|�B
v�B
jB
D�B
�B	��B	��B	�B	�;B	�)B	�B	��B	�B	�HB	��B	�!B	��B	q�B	YB	L�B	B�B	8RB	/B	&�B	�B	�B	{B	+B��B��B��B��B��B��B��B��B��B��B��B��B�B�;B�B��B��B�wB�^B�9B�9B�9B�3B�-B�'B�B�B��B��B��B��B�{B�bB�JB�DB�7B�+B�B�B�B}�B|�Bx�Bv�Bu�Bt�Bs�Br�Bq�Bp�Bp�Bm�Bn�Bn�Bl�Bk�BiyBgmBe`BcTBaHB^5B]/B\)B[#BZBYBXBW
BVBT�BS�BQ�BN�BJ�BH�BG�BG�BE�BF�BE�BE�BD�BC�BA�B@�B?}B>wB=qB:^B7LB8RB7LB6FB33B1'B/B/B-B,B/B@�BE�BE�BC�BB�BB�BA�BC�BE�BD�BD�BE�BD�BC�BH�BL�BL�BP�BgmBt�Bp�BffBhsBx�B}�B� B� B� B�B�B�+B�+B�%B�B�+B�=B�DB�DB�VB�PB�PB�bB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�-B�?B�FB�LB�RB�XB�jB�qB�qB��BBÖBĜBĜBĜBŢBĜBǮBɺB��B��B��B�B�B�B�B�)B�)B�)B�)B�5B�;B�;B�TB�mB�B�B�B��B��B��B��B��B��B��B��B	B	B	+B	1B	
=B	\B	oB	oB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	&�B	'�B	-B	.B	2-B	5?B	7LB	7LB	7LB	>wB	C�B	D�B	G�B	I�B	K�B	K�B	L�B	N�B	Q�B	S�B	VB	YB	^5B	`BB	cTB	ffB	iyB	jB	k�B	m�B	n�B	n�B	p�B	r�B	t�B	v�B	w�B	x�B	y�B	{�B	|�B	}�B	~�B	�B	�B	�B	�1B	�7B	�DB	�PB	�bB	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�-B	�?B	�FB	�FB	�FB	�RB	�^B	�jB	�jB	�qB	�qB	�qB	�}B	��B	B	B	ÖB	ĜB	�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B>wB>wB>wB>wB>wB>wB>wB?}B?}B>wB>wB>wB>wB@�BB�BB�BD�BD�BE�BE�BF�BF�BF�BK�BD�BB�BXBv�Bx�B~�B�+B�1B�DB�hB��B��B��B��B��B�RBȴB��B�
B�)B�BB�B  B1BVBhB�B�B!�B#�B%�B%�B'�B'�B(�B+B,B-B.B,B+B1'B1'B2-B33B49B6FB7LB9XB:^B=qB=qB;dB6FB2-B/B-B'�B �B�B{BhBJBJB%B��B��B�B�TB�NB�BB�)B��B��BƨB�wB�?B�B��B�\B}�Bl�BbNB[#BQ�BK�BE�B?}B(�BuB+BB  B��BĜB�B��B��B��B�7B�B|�Bn�BM�BD�B;dB-B�BB
�B
�5B
ɺB
�!B
��B
�1B
�B
y�B
s�B
M�B
!�B	��B	��B	�B	�HB	�)B	��B
B	��B	�sB	�B	�RB	��B	~�B	`BB	Q�B	H�B	?}B	49B	-B	$�B	�B	"�B	JB	B��B��B��B	B	  B��B��B��B��B��B��B��B�NB�;B��BB��BB�LB�?B�FB�FB�9B�?B�B�B�B��B��B��B��B�{B�VB�PB�JB�=B�1B�B�B�B� B� Bz�Bw�Bu�Bu�Bu�Bv�Br�Bq�Bp�Bp�Bp�Bo�Bn�Bk�BjBgmBe`BdZBbNB_;B^5B\)B[#B[#B[#BXBW
BVBT�BS�BS�BO�BJ�BJ�BI�BG�BG�BF�BE�BE�BE�BE�BC�B@�B?}B?}BA�B=qB;dB8RB7LB8RB5?B2-B1'B0!B/B/BA�BH�BH�BE�BF�BD�BF�BE�BF�BE�BD�BF�BF�BC�BH�BM�BM�BM�Be`Bv�Bt�BgmBffBz�B� B�B�B�B�B�%B�7B�1B�+B�1B�7B�DB�JB�\B�oB�\B�VB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�9B�FB�LB�RB�XB�^B�qB�wB�}B��BÖBĜBĜBĜBĜBŢBƨBɺB��B��B��B��B�
B�B�B�#B�/B�/B�/B�/B�;B�BB�HB�ZB�sB�B�B�B��B��B��B��B��B��B��B��B	B	B	+B		7B	DB	bB	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	&�B	'�B	(�B	.B	/B	2-B	6FB	7LB	8RB	8RB	?}B	C�B	E�B	G�B	I�B	K�B	K�B	L�B	O�B	R�B	S�B	VB	ZB	_;B	aHB	dZB	gmB	iyB	jB	l�B	m�B	n�B	n�B	q�B	r�B	t�B	v�B	w�B	x�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�B	�1B	�7B	�DB	�PB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�?B	�FB	�FB	�LB	�XB	�^B	�jB	�jB	�qB	�qB	�wB	��B	��B	B	B	ÖB	ĜB	�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
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
<�o<T��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446432012010314464320120103144643  AO  ARGQ                                                                        20111130135707  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135707  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144643  IP                  G�O�G�O�G�O�                