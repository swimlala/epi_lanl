CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:30Z UW 3.1 conversion   
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142340  20190522121827  1727_5046_140                   2C  D   APEX                            2143                            040306                          846 @�ʚ�I��1   @�ʛF)��@7^�Q��c�(�\1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A>ffA^ffA�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL�CN�CP�CR  CT  CV  CX  CY�fC[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D y�D ��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�  A   A   A>ffA^ffA�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL�CN�CP�CR  CT  CV  CX  CY�fC[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D y�D ��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�XA�XA�VA�ZA�\)A�\)A�`BA�bNA�bNA�dZA�dZA�dZA�bNA�C�A��`A��^A��+A� �A�ƨA��jA���A��7A�dZA�\)A�^5A�jA�hsA�hsA�l�A�p�A�p�A�x�A�~�A��uA���A��+A�z�A�jA��A��hA���A�G�A�&�A� �A��yA��FA�p�A��!A��A�r�A�/A��jA� �A�p�A�+A��#A�/A��A���A�z�A�l�A�hsA�1'A���A��TA�jA���A�x�A�O�A�?}A�9XA��A���A�C�A���A��PA�5?A���A���A�%A��uA�ȴA�=qA�dZA�A�A��;A��hA��yA�dZA�dZA���A�O�A��mA��7A�9XA��A�`BA�l�A��7A� �A�~�A��A��+A��A��A�hsA�K�A��`A�E�A�dZA�/A���A�ȴA��9A��uA�33A��DA�5?A���A�|�A���A��wA~�yA|��A{�TAzI�AxAwK�Aw%Avn�AuVAs%Aq�#ApE�AnI�Am|�Akt�AjQ�AiAhVAhJAf��Ad�9Ab�Aa�
Aa;dA`^5A`1A_��A_��A^ �A\ �A[�TAZ��AYdZAX��AW��AW�AT��AS��AS�AR��AQ/AO��ANjAL�9ALJAKG�AI��AH��AHQ�AG�AG��AF�\AE��AD�9AB��A@ZA>��A>(�A=�A=�A<��A;S�A:�RA:��A:M�A9�FA8�A8v�A7;dA5XA4�A4�RA3�mA3"�A2�!A1��A0�jA/�A.��A-��A,��A,v�A+�7A*��A*9XA(�/A'�wA%�;A$��A#��A"�DA"5?A!��A ffAp�A�/A�wA��A��A�^A\)AoA��A~�A9XA��Ar�A��A��AA�A�A�A��A�uA�PAVA��A33A
�DA
=qA
JA	�TA	�AffAhsA�A�AQ�AXAn�AAO�A ~�@��@���@�b@��@�=q@�`B@���@���@��@���@�@�\@�j@�v�@�G�@���@��@��@���@�V@��#@��y@��@�I�@ڏ\@� �@�=q@�O�@Ԭ@ӕ�@��@�x�@�X@�Q�@���@���@˾w@��@�E�@�@�x�@�hs@��@�r�@�  @��@���@�  @��@� �@� �@� �@��
@�l�@Ə\@ũ�@���@�dZ@�@���@�b@�K�@��!@��@���@���@��@�v�@�hs@���@�?}@�/@��`@��9@�J@���@�
=@���@�n�@�{@��`@�1'@��m@��@�M�@��@��@��/@�Z@�o@��^@�G�@�Ĝ@��
@���@�-@�`B@��j@�j@��P@���@�{@��/@���@��m@��@��@�b@��;@�ƨ@��P@��y@�@�V@���@���@��y@��T@�X@���@��j@�r�@�1'@��@��F@��@�C�@��@���@�n�@�M�@�5?@��@���@�`B@��@���@�bN@�(�@��@�1@��;@��w@���@�C�@�@��@�ȴ@�v�@�J@���@�p�@�&�@��@�V@�V@���@��@�r�@�(�@�b@���@��;@��F@��@�|�@�\)@���@��!@�n�@�5?@�@���@���@�J@��H@��@�n�@��@�?}@��@���@�1'@���@�ƨ@��P@�"�@��R@�~�@�n�@�n�@�n�@�E�@�E�@���@��-@���@��9@�j@�(�@�b@��@�ƨ@�l�@�33@�t�@�l�@��P@��@�Q�@���@�bN@�(�@�(�@�9X@��;@��w@�+@��@�n�@�J@�J@��@���@�p�@�G�@��@��@���@��j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�XA�XA�VA�ZA�\)A�\)A�`BA�bNA�bNA�dZA�dZA�dZA�bNA�C�A��`A��^A��+A� �A�ƨA��jA���A��7A�dZA�\)A�^5A�jA�hsA�hsA�l�A�p�A�p�A�x�A�~�A��uA���A��+A�z�A�jA��A��hA���A�G�A�&�A� �A��yA��FA�p�A��!A��A�r�A�/A��jA� �A�p�A�+A��#A�/A��A���A�z�A�l�A�hsA�1'A���A��TA�jA���A�x�A�O�A�?}A�9XA��A���A�C�A���A��PA�5?A���A���A�%A��uA�ȴA�=qA�dZA�A�A��;A��hA��yA�dZA�dZA���A�O�A��mA��7A�9XA��A�`BA�l�A��7A� �A�~�A��A��+A��A��A�hsA�K�A��`A�E�A�dZA�/A���A�ȴA��9A��uA�33A��DA�5?A���A�|�A���A��wA~�yA|��A{�TAzI�AxAwK�Aw%Avn�AuVAs%Aq�#ApE�AnI�Am|�Akt�AjQ�AiAhVAhJAf��Ad�9Ab�Aa�
Aa;dA`^5A`1A_��A_��A^ �A\ �A[�TAZ��AYdZAX��AW��AW�AT��AS��AS�AR��AQ/AO��ANjAL�9ALJAKG�AI��AH��AHQ�AG�AG��AF�\AE��AD�9AB��A@ZA>��A>(�A=�A=�A<��A;S�A:�RA:��A:M�A9�FA8�A8v�A7;dA5XA4�A4�RA3�mA3"�A2�!A1��A0�jA/�A.��A-��A,��A,v�A+�7A*��A*9XA(�/A'�wA%�;A$��A#��A"�DA"5?A!��A ffAp�A�/A�wA��A��A�^A\)AoA��A~�A9XA��Ar�A��A��AA�A�A�A��A�uA�PAVA��A33A
�DA
=qA
JA	�TA	�AffAhsA�A�AQ�AXAn�AAO�A ~�@��@���@�b@��@�=q@�`B@���@���@��@���@�@�\@�j@�v�@�G�@���@��@��@���@�V@��#@��y@��@�I�@ڏ\@� �@�=q@�O�@Ԭ@ӕ�@��@�x�@�X@�Q�@���@���@˾w@��@�E�@�@�x�@�hs@��@�r�@�  @��@���@�  @��@� �@� �@� �@��
@�l�@Ə\@ũ�@���@�dZ@�@���@�b@�K�@��!@��@���@���@��@�v�@�hs@���@�?}@�/@��`@��9@�J@���@�
=@���@�n�@�{@��`@�1'@��m@��@�M�@��@��@��/@�Z@�o@��^@�G�@�Ĝ@��
@���@�-@�`B@��j@�j@��P@���@�{@��/@���@��m@��@��@�b@��;@�ƨ@��P@��y@�@�V@���@���@��y@��T@�X@���@��j@�r�@�1'@��@��F@��@�C�@��@���@�n�@�M�@�5?@��@���@�`B@��@���@�bN@�(�@��@�1@��;@��w@���@�C�@�@��@�ȴ@�v�@�J@���@�p�@�&�@��@�V@�V@���@��@�r�@�(�@�b@���@��;@��F@��@�|�@�\)@���@��!@�n�@�5?@�@���@���@�J@��H@��@�n�@��@�?}@��@���@�1'@���@�ƨ@��P@�"�@��R@�~�@�n�@�n�@�n�@�E�@�E�@���@��-@���@��9@�j@�(�@�b@��@�ƨ@�l�@�33@�t�@�l�@��P@��@�Q�@���@�bN@�(�@�(�@�9X@��;@��w@�+@��@�n�@�J@�J@��@���@�p�@�G�@��@��@���@��j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBƨBƨBƨBƨBƨBƨBƨBƨBƨBǮBǮBȴBƨB��B�B�/B�HB�yB�B�B�B�B�yB�yB�B�B�B�B��B��B��B��B��B��BB��B��BBB
=B�B)�B2-B?}BE�BC�BC�BE�BJ�BJ�BI�BI�BI�BN�BS�BO�B>wBA�BD�BD�BF�BF�BH�BI�B?}B6FB5?B2-B1'B2-B<jBQ�BN�B0!B(�B�BB�B�B�/B�dB�B��Bw�BR�BC�B:^B/B&�B�B\B	7BB��B��B�B��B�dB��B}�Bv�Br�Bm�BiyBaHBP�B<jB"�B
��B
�;B
�)B
�B
�
B
��B
��B
��B
��B
�'B
��B
��B
�B
y�B
p�B
iyB
dZB
ZB
O�B
K�B
H�B
C�B
<jB
-B
'�B
�B
{B
VB
B
B	��B	��B	�B	�sB	�)B	��B	��B	ȴB	ŢB	ÖB	B	�}B	�LB	�B	�B	��B	��B	��B	��B	��B	��B	�hB	�VB	�=B	�B	}�B	r�B	k�B	gmB	cTB	_;B	[#B	YB	VB	R�B	M�B	H�B	B�B	9XB	0!B	'�B	$�B	"�B	�B	�B	�B	�B	{B	oB	\B	PB	
=B	B	  B��B��B��B��B�B�B�`B�HB�/B�B�B��B��BǮBÖB��B�dB�LB�9B�!B�B��B��B��B��B��B�oB�7B�B�B� B~�B}�B}�B{�B{�Bs�Bp�Bo�Bm�Bm�Bk�BjBhsBffBdZBcTBbNBbNBaHBaHB`BB^5B\)BYBT�BQ�BO�BM�BL�BK�BJ�BH�BG�BF�BF�BF�BD�BF�BE�BD�BA�BA�BB�BC�BC�BB�BA�BB�B@�B?}B>wB=qB;dB=qB<jB?}B<jB<jB<jB<jB<jB<jB?}BD�BG�BJ�BJ�BK�BN�BR�BR�BQ�BR�BR�BQ�BQ�BT�BW
BXBYBZB[#B\)B^5B_;B_;BaHBdZBffBiyBm�Bo�Bp�Br�Bs�Bt�Bu�By�By�Bz�B}�B� B�%B�DB�JB�=B�=B�DB�\B�oB�oB�uB��B��B��B��B��B��B��B��B��B��B�B�B�!B�9B�RB�jB��BÖBǮBɺB��B�B��B�B�#B�;B�HB�TB�`B�`B�mB�B�B��B��B��B��B	B	1B		7B	DB	VB	bB	uB	�B	�B	�B	�B	�B	!�B	"�B	$�B	'�B	)�B	.B	0!B	2-B	6FB	8RB	8RB	9XB	:^B	<jB	>wB	A�B	C�B	C�B	D�B	F�B	I�B	K�B	L�B	M�B	M�B	N�B	N�B	O�B	R�B	T�B	ZB	\)B	]/B	`BB	bNB	e`B	gmB	iyB	jB	l�B	n�B	r�B	t�B	u�B	u�B	x�B	~�B	�B	�B	�B	�%B	�7B	�1B	�+B	�+B	�+B	�+B	�7B	�PB	�bB	�hB	�oB	�oB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�FB	�FB	�RB	�dB	�jB	�wB	��B	B	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BƨBƨBƨBƨBƨBƨBƨBƨBƨBǮBǮBȴBǮB��B�B�5B�TB�B�B�B�B�B�yB�yB�B�B�B�B��B��B��B��B��B��BB��B��BB+BPB�B+B2-B@�BF�BD�BF�BF�BJ�BK�BK�BL�BM�BO�BVBS�B?}BC�BE�BD�BF�BH�BL�BN�BB�B:^B7LB33B1'B2-B=qBT�BXB49B/B�B+B�B�B�TB�wB�!B��B� BYBD�B=qB1'B,B�BhBDBB��B��B��B��BǮB�?B�Bx�Bu�Bo�BjBe`BS�B?}B.B
��B
�BB
�/B
�B
�
B
�B
��B
��B
ǮB
�LB
��B
��B
�DB
� B
u�B
l�B
hsB
`BB
Q�B
L�B
J�B
G�B
B�B
1'B
-B
%�B
�B
{B
	7B
B	��B	��B	��B	�B	�NB	��B	��B	��B	ƨB	ÖB	ĜB	ĜB	�qB	�B	�!B	�B	��B	��B	��B	��B	��B	�oB	�\B	�VB	�B	�B	w�B	m�B	iyB	gmB	bNB	]/B	ZB	W
B	VB	P�B	K�B	H�B	?}B	49B	)�B	&�B	#�B	!�B	�B	�B	�B	�B	{B	oB	\B	VB	DB	B��B��B��B��B��B�B�yB�ZB�HB�#B�B�B��BɺBȴBĜB��B�jB�RB�9B�B�B�B��B��B��B��B�VB�B�B�B� B~�B~�B}�B~�Bz�Bs�Bq�Bp�Bq�Bm�Bk�Bk�BhsBhsBe`BdZBcTBbNBbNBcTBaHB_;B]/BW
BVBR�BP�BN�BN�BM�BK�BG�BF�BH�BG�BF�BJ�BI�BI�BC�BC�BD�BF�BG�BD�BC�BD�BB�BB�B@�B>wB>wB>wB>wBA�B?}B>wB=qB=qB>wB>wB@�BD�BI�BK�BM�BN�BP�BS�BS�BR�BR�BS�BQ�BR�BT�BW
BXBYBZB[#B\)B_;B`BBaHBcTBdZBiyBl�Bo�Bq�Br�Bs�Bu�Bv�Bw�By�Bz�B|�B~�B� B�%B�JB�PB�=B�VB�PB�bB�uB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�FB�XB�qB��BĜBȴB��B��B�B��B�B�#B�;B�HB�TB�`B�fB�sB�B�B��B��B��B��B	B		7B	
=B	JB	\B	hB	{B	�B	�B	�B	�B	�B	!�B	"�B	%�B	'�B	+B	.B	0!B	33B	6FB	8RB	8RB	9XB	:^B	<jB	?}B	A�B	C�B	C�B	E�B	G�B	J�B	K�B	M�B	M�B	M�B	N�B	N�B	P�B	R�B	VB	ZB	\)B	]/B	`BB	bNB	e`B	gmB	jB	k�B	l�B	n�B	r�B	t�B	u�B	u�B	w�B	~�B	�B	�B	�B	�%B	�=B	�7B	�+B	�+B	�+B	�1B	�=B	�PB	�bB	�hB	�oB	�oB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�FB	�FB	�RB	�dB	�qB	�wB	��B	ÖB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<��
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447232012010314472320120103144723  AO  ARGQ                                                                        20111130142340  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142340  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144723  IP                  G�O�G�O�G�O�                