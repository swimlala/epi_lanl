CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:10Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               FA   AO  20111130140718  20190522121826  1727_5046_070                   2C  D   APEX                            2143                            040306                          846 @�p"��1   @�p#-��@7Rn��O��c��j~��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A���A�  A���A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsl�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  A   A   A@  A`  A�  A���A�  A���A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsl�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��\A��\A��uA���A���A���A���A���A���A���A���A���A���A��\A�hsA�`BA�Q�A�K�A�=qA�-A���A��jA�/A�Q�A���A�"�A���A���A��A�A��A�ȴA��RA���A�?}A�v�A�;dA���A�XA�  A�=qA�M�A��RA�jA�A���A�jA�+A���A�ĜA��+A�?}A�-A��A��uA�A�A�l�A�S�A��A���A�O�A��A��HA�A�A��A�n�A���A��9A���A�^5A�+A��RA���A�r�A��A��-A��9A�G�A���A��/A�ZA���A�^5A��;A�A�jA�S�A�jA��A�ĜA�bA��;A�K�A��A��hA� �A�;dA��A��A�S�A�7LA��A��TA�\)A�JA���A�?}A�K�A��jA�|�A�~�A�G�A��A�E�A���A�dZA���A|ffAzz�Ax��Au�7As33AoS�An~�Am7LAi�Ag�;Af��AfffAe�Ac�;Ab�9A]`BAZ��AY��AX�AW��AV1AT�jAS��AR�/AQ��AQ��AQ��AQ\)AQ"�APA�ANM�AM�PAM�AM%AL�uAKt�AJbAI7LAI�AG��AF(�AD��AC;dAA\)A@�A>��A=O�A<�A<{A:��A9�TA9K�A8��A8��A8v�A8-A6��A5��A4�uA3XA2��A2��A2=qA1�A0�A/��A.��A.�DA.ZA.-A-�#A-%A,�RA,=qA+�A)�A(�HA(~�A(bA't�A&�9A%`BA$~�A#��A"~�A"A!�wA!&�A�7A�A��AM�A��AffA��A�A�AZA��A�A�Az�AM�A��A��A�9A�7A �A�A
�!A	
=A-AhsA��Ax�A-AJA�AO�A+A V@��\@���@�+@��^@��9@��@�E�@�J@�b@�p�@���@�ȴ@�%@�bN@�J@���@�{@�G�@�j@��y@��T@�  @ާ�@�&�@���@٩�@ش9@���@�K�@֟�@�Ĝ@�l�@���@�%@�Ĝ@У�@�Ĝ@д9@�I�@�|�@Η�@���@��`@�7L@̬@�dZ@ˮ@�o@ʏ\@��@ȓu@��y@�X@�1@�t�@�"�@§�@��@��h@��/@�\)@�C�@��@�-@�X@��9@���@�1@���@��@��
@��+@���@�Ĝ@�"�@�G�@��u@�Z@��@�K�@�^5@�x�@��@��/@��/@��@�Ĝ@��`@�Q�@�E�@���@���@�;d@�o@���@��F@�l�@���@�{@�{@��y@��;@�V@�-@��@�@�;d@��@���@���@�n�@�v�@��+@��\@�v�@��@��@��@�r�@�l�@���@�@�G�@���@��H@��@�hs@���@�(�@�S�@��@�~�@�E�@��@�@��^@��@�@��R@���@�ff@�$�@��@��@�/@��@��@���@���@��;@��w@�dZ@��@��!@���@���@��@��H@��!@��@�O�@�9X@��m@�t�@��@��\@���@�x�@��@��@�1@��m@�  @�ƨ@�=q@�M�@��@�{@��h@�&�@�%@��@��/@��9@�j@��w@�S�@�33@��@���@�n�@�M�@�5?@��T@�hs@�&�@��9@�bN@�  @���@�\)@�C�@�"�@�@��@��R@��!@���@��\@�~�@��\@�M�@�$�@�@��7@�G�@�7L@��@���@�Ĝ@��9@��@�bN@�Z@�1'@��@�  @�  @��;@��
@�ƨ@��F@�K�@�"�@�ȴ@���@��R@��!@���@���@���@���@�~�@�-@��h@�?}@�%@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��\A��\A��uA���A���A���A���A���A���A���A���A���A���A��\A�hsA�`BA�Q�A�K�A�=qA�-A���A��jA�/A�Q�A���A�"�A���A���A��A�A��A�ȴA��RA���A�?}A�v�A�;dA���A�XA�  A�=qA�M�A��RA�jA�A���A�jA�+A���A�ĜA��+A�?}A�-A��A��uA�A�A�l�A�S�A��A���A�O�A��A��HA�A�A��A�n�A���A��9A���A�^5A�+A��RA���A�r�A��A��-A��9A�G�A���A��/A�ZA���A�^5A��;A�A�jA�S�A�jA��A�ĜA�bA��;A�K�A��A��hA� �A�;dA��A��A�S�A�7LA��A��TA�\)A�JA���A�?}A�K�A��jA�|�A�~�A�G�A��A�E�A���A�dZA���A|ffAzz�Ax��Au�7As33AoS�An~�Am7LAi�Ag�;Af��AfffAe�Ac�;Ab�9A]`BAZ��AY��AX�AW��AV1AT�jAS��AR�/AQ��AQ��AQ��AQ\)AQ"�APA�ANM�AM�PAM�AM%AL�uAKt�AJbAI7LAI�AG��AF(�AD��AC;dAA\)A@�A>��A=O�A<�A<{A:��A9�TA9K�A8��A8��A8v�A8-A6��A5��A4�uA3XA2��A2��A2=qA1�A0�A/��A.��A.�DA.ZA.-A-�#A-%A,�RA,=qA+�A)�A(�HA(~�A(bA't�A&�9A%`BA$~�A#��A"~�A"A!�wA!&�A�7A�A��AM�A��AffA��A�A�AZA��A�A�Az�AM�A��A��A�9A�7A �A�A
�!A	
=A-AhsA��Ax�A-AJA�AO�A+A V@��\@���@�+@��^@��9@��@�E�@�J@�b@�p�@���@�ȴ@�%@�bN@�J@���@�{@�G�@�j@��y@��T@�  @ާ�@�&�@���@٩�@ش9@���@�K�@֟�@�Ĝ@�l�@���@�%@�Ĝ@У�@�Ĝ@д9@�I�@�|�@Η�@���@��`@�7L@̬@�dZ@ˮ@�o@ʏ\@��@ȓu@��y@�X@�1@�t�@�"�@§�@��@��h@��/@�\)@�C�@��@�-@�X@��9@���@�1@���@��@��
@��+@���@�Ĝ@�"�@�G�@��u@�Z@��@�K�@�^5@�x�@��@��/@��/@��@�Ĝ@��`@�Q�@�E�@���@���@�;d@�o@���@��F@�l�@���@�{@�{@��y@��;@�V@�-@��@�@�;d@��@���@���@�n�@�v�@��+@��\@�v�@��@��@��@�r�@�l�@���@�@�G�@���@��H@��@�hs@���@�(�@�S�@��@�~�@�E�@��@�@��^@��@�@��R@���@�ff@�$�@��@��@�/@��@��@���@���@��;@��w@�dZ@��@��!@���@���@��@��H@��!@��@�O�@�9X@��m@�t�@��@��\@���@�x�@��@��@�1@��m@�  @�ƨ@�=q@�M�@��@�{@��h@�&�@�%@��@��/@��9@�j@��w@�S�@�33@��@���@�n�@�M�@�5?@��T@�hs@�&�@��9@�bN@�  @���@�\)@�C�@�"�@�@��@��R@��!@���@��\@�~�@��\@�M�@�$�@�@��7@�G�@�7L@��@���@�Ĝ@��9@��@�bN@�Z@�1'@��@�  @�  @��;@��
@�ƨ@��F@�K�@�"�@�ȴ@���@��R@��!@���@���@���@���@�~�@�-@��h@�?}@�%@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB33B33B33B49B33B49B5?B7LB7LB6FB9XB8RB:^B=qBM�BQ�BVBXB[#B^5BgmBq�B�=B�-B��B��B�HB�sB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�sB�fB�ZB�HB�)B�B�B�B��BÖB�wB�dB�FB�-B�B��B��B��B�hB�bB�JB�Bl�BO�BF�B<jB49B0!B'�B�BPB��B�B��B��B�B��B�JB|�Bs�BgmB[#BT�BI�B?}B49B0!B,B�BB
��B
��B
�B
�;B
��B
ɺB
ƨB
ŢB
��B
�B
��B
�VB
�7B
|�B
T�B
F�B
9XB
#�B
oB	��B	�B	�BB	��B	�}B	ƨB	ĜB	�qB	�-B	�B	�=B	p�B	jB	cTB	^5B	T�B	L�B	G�B	A�B	A�B	I�B	VB	_;B	hsB	gmB	`BB	[#B	VB	VB	P�B	I�B	@�B	@�B	C�B	;dB	2-B	(�B	�B	oB	DB	B��B��B��B��B�B�B�B�B�B�sB�TB�;B�B�B��B��B��B��B��BǮBƨBŢBĜBÖBB��B�wB�dB�LB�3B�'B�!B�B�B��B��B��B��B��B��B��B��B�{B�bB�VB�PB�=B�7B�1B�%B�B�B�B~�Bz�Bu�Br�Bp�Bm�BiyBffBcTBbNB_;B]/B[#BZBW
BS�BO�BO�BN�BN�BL�BJ�BJ�BH�BF�BF�BE�BD�BE�BE�BB�BB�BB�B@�BB�BA�BB�BA�B@�B?}B=qB<jB;dB;dB:^B8RB7LB6FB6FB7LB7LB6FB6FB6FB5?B9XB:^B<jB>wB>wB>wBC�BD�BC�BG�BK�BJ�BQ�BYB[#BZBYBW
BW
BXB[#B\)B\)B]/B]/B_;B`BBhsBiyBiyBk�Bn�Br�Bs�Bs�Bq�Br�Bt�Bv�Bw�Bw�Bu�Bv�Bv�Bv�Bx�B{�B{�B~�B�B�+B�=B�PB�oB��B��B��B��B��B��B��B��B�3B�3B�-B�FB�^B��BɺB��B�B�5B�BB�NB�fB�sB�B�B�B�B�B��B��B��B��B��B	  B	B	B	%B	%B	JB	bB	uB	�B	�B	�B	�B	 �B	!�B	"�B	"�B	"�B	&�B	/B	2-B	33B	33B	49B	5?B	8RB	:^B	;dB	;dB	<jB	>wB	H�B	J�B	L�B	N�B	Q�B	R�B	S�B	W
B	ZB	ZB	[#B	ZB	]/B	`BB	bNB	dZB	cTB	aHB	aHB	aHB	bNB	e`B	gmB	iyB	k�B	n�B	q�B	r�B	u�B	z�B	}�B	� B	�B	�B	�B	�B	�+B	�7B	�=B	�DB	�PB	�VB	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�3B	�?B	�FB	�FB	�LB	�XB	�XB	�dB	�jB	�jB	�qB	�wB	�wB	�}B	�}B	��B	��B	��B	ÖB	ĜB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B33B33B33B49B33B49B5?B7LB7LB6FB9XB8RB:^B>wBM�BQ�BVBXB[#B_;BhsBt�B�\B�FB��B�
B�`B�B�sB�sB�B�B�B�B�B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�sB�yB�mB�5B�#B�#B�B�)BǮB��B�wB�XB�-B�-B�B��B��B�hB�hB�VB�+Bt�BQ�BI�B?}B6FB2-B)�B#�BoB%B��B��BɺB�'B��B�uB� Bx�Bl�B^5B[#BP�BB�B5?B2-B7LB"�B1B  B
��B
��B
�`B
��B
��B
ƨB
ƨB
ŢB
�?B
��B
�\B
�DB
�\B
ZB
J�B
B�B
+B
�B	��B	�B	�sB	��B	B	ȴB	ȴB	��B	�FB	�jB	�oB	s�B	l�B	gmB	cTB	YB	O�B	J�B	D�B	B�B	J�B	W
B	`BB	k�B	m�B	cTB	]/B	VB	XB	T�B	M�B	B�B	@�B	F�B	?}B	5?B	-B	!�B	�B	\B	%B	  B��B��B��B�B�B�B�B�B�B�fB�NB�/B�B�B��B��B��B��B��BǮBƨBŢBĜBŢB��B��B�}B�dB�LB�-B�-B�!B�B�B��B��B��B��B��B��B��B��B��B�hB�bB�VB�JB�=B�+B�+B�B�B�B~�B{�Bt�Br�Br�Bl�BjBffBcTBcTB_;B]/B\)BZBXBVBQ�BO�BO�BO�BM�BL�BL�BH�BH�BG�BF�BF�BI�BG�BE�BD�BC�BC�BE�BF�BC�BA�B@�B?}B>wB>wB=qB<jB;dB9XB8RB7LB8RB8RB9XB8RB7LB8RB:^B:^B<jB>wB?}B?}BE�BE�BD�BG�BL�BL�BQ�BZB\)B[#B[#BZBYBZB\)B]/B]/B^5B^5B`BBbNBhsBiyBk�Bl�Bo�Br�Bt�Bu�Br�Bv�Bv�Bx�By�Bz�Bx�Bw�Bw�Bw�Bz�B}�B}�B� B�B�+B�=B�PB�oB��B��B��B��B��B��B��B��B�9B�9B�3B�FB�XB��BǮB��B�B�5B�BB�TB�fB�sB�B�B�B�B�B��B��B��B��B��B	B	B	%B	+B		7B	VB	hB	{B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	$�B	)�B	0!B	2-B	33B	49B	5?B	6FB	9XB	:^B	;dB	;dB	=qB	?}B	H�B	K�B	M�B	N�B	Q�B	R�B	S�B	W
B	ZB	[#B	]/B	\)B	^5B	aHB	cTB	e`B	dZB	bNB	bNB	bNB	cTB	e`B	gmB	iyB	m�B	n�B	q�B	r�B	v�B	{�B	}�B	� B	�B	�B	�B	�B	�1B	�7B	�DB	�JB	�PB	�VB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�?B	�FB	�FB	�RB	�XB	�XB	�dB	�jB	�jB	�qB	�wB	�wB	�}B	�}B	��B	��B	��B	ÖB	ŢB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�t�<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446582012010314465820120103144658  AO  ARGQ                                                                        20111130140718  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140718  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144658  IP                  G�O�G�O�G�O�                