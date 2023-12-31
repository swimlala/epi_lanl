CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-16T06:37:02Z creation;2019-05-16T06:37:08Z conversion to V3.1;2019-12-23T06:02:15Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20190516063702  20200120031518  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_146                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ؾHd� 1   @ؾH��� @7��O�;d�b���v�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	�fD
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dyy�Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�P 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��A
�\A*�\AJ�\Aj�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B
��B��B��B"��B*��B2��B:��BB��BJ��BR��BZ��Bb��Bj��Br��Bz��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�aHC�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{C�T{D *=D �=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D	*=D	��D
*=D
�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D*=D�=D *=D �=D!*=D!�=D"*=D"�=D#*=D#�=D$*=D$�=D%*=D%�=D&*=D&�=D'*=D'�=D(*=D(�=D)*=D)�=D**=D*�=D+*=D+�=D,*=D,�=D-*=D-�=D.*=D.�=D/*=D/�=D0*=D0�=D1*=D1�=D2*=D2�=D3*=D3�=D4*=D4�=D5*=D5�=D6*=D6�=D7*=D7�=D8*=D8�=D9*=D9�=D:*=D:�=D;*=D;�=D<*=D<�=D=*=D=�=D>*=D>�=D?*=D?�=D@*=D@�=DA*=DA�=DB*=DB�=DC*=DC�=DD*=DD�=DE*=DE�=DF*=DF�=DG*=DG�=DH*=DH�=DI*=DI�=DJ*=DJ�=DK*=DK�=DL*=DL�=DM*=DM�=DN*=DN�=DO*=DO�=DP*=DP�=DQ*=DQ�=DR*=DR�=DS*=DS�=DT*=DT�=DU*=DU�=DV*=DV�=DW*=DW�=DX*=DX�=DY*=DY�=DZ*=DZ�=D[*=D[�=D\*=D\�=D]*=D]�=D^*=D^�=D_*=D_�=D`*=D`�=Da*=Da�=Db*=Db�=Dc*=Dc�=Dd*=Dd�=De*=De�=Df*=Df�=Dg*=Dg�=Dh*=Dh�=Di*=Di�=Dj*=Dj�=Dk*=Dk�=Dl*=Dl�=Dm*=Dm�=Dn*=Dn�=Do*=Do�=Dp*=Dp�=Dq*=Dq�=Dr*=Dr�=Ds*=Ds�=Dt*=Dt�=Du*=Du�=Dv*=Dv�=Dw*=Dw�=Dx*=Dx�=Dy*=Dy��Dz*=Dz�=D{*=D{�=D|*=D|�=D}*=D}�=D~*=D~�=D*=D�=D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��RD��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UDD��D�D�UDÕD��D�D�UDĕD��D�D�UDŕD��D�D�UDƕD��D�D�UDǕD��D�D�UDȕD��D�D�UDɕD��D�D�UDʕD��D�D�UD˕D��D�D�UD̕D��D�D�UD͕D��D�D�UDΕD��D�D�UDϕD��D�D�UDЕD��D�D�UDѕD��D�D�UDҕD��D�D�UDӕD��D�D�UDԕD��D�D�UDՕD��D�D�UD֕D��D�D�UDוD��D�D�UDؕD��D�D�UDٕD��D�D�UDڕD��D�D�UDەD��D�D�UDܕD��D�D�UDݕD��D�D�UDޕD��D�D�UDߕD��D�D�UD��D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD�D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�D�UD��D��D�RD�e111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AɋDA�=qA���AǾwA�|�A��A�ȴAƙ�A�n�A�?}A�$�A�bNA�VA�S�A���A�z�A�bA�
=A�ZA�
=A�v�A�ZA�A�A���A�`BA��hA�1A�A�1'A��PA�jA�ZA��A�=qA�VA��!A��/A�M�A��A�r�A���A�=qA��HA��/A���A���A�C�A���A�n�A�A�VA��A���A�A�dZA��
A�I�A��A���A�^5A��A���A��A�%A�;dA�Q�A���A��wA��A���A�Q�A���A�+A���A�=qA��A��`A��A��!A�A�A�v�A���A��A�z�A�bNA��;A�ZA���A�ZA��-A��A�JA��`A�n�A���A�9XA��\A��A���A��A��HA�/A�I�A�ZA��mA���A���A��A~1A{ƨAyAvĜAuO�Au/As�FAo�FAl��Aj�AhM�Ae��Ab=qA]�AZ�jAX-AWt�AV=qAS��AR�HARA�AQ�-AQ?}AP�HAN�!AK�AJJAI�AI�^AI/AG�mAFv�AEABbA@E�A?��A?��A?oA<�A:�yA:ffA:A�A8�!A7oA6ffA5��A4 �A2JA1K�A0��A01'A.��A-dZA+�TA*�A)ƨA(E�A'�-A'�A&�A%�FA$ZA#oA"9XA!XA ��A 1'AhsA�AjA��A�RA�A�Ax�A|�AS�A��Ap�AE�A�A �AdZA��A�jAv�A�#A��A��A�A�#AVA
5?A	��A�A�TA��A?}Az�A�A�Ar�A ��A �@��@��@��@���@��D@�9X@�dZ@��R@�ff@��@�33@��@�Q�@�F@�@�`B@��D@�1@@�v�@�bN@�R@�I�@��H@�p�@�bN@�@�E�@�p�@�I�@��y@�E�@�G�@ܓu@� �@��@�X@��
@��@�I�@��@�=q@���@Ѓ@�\)@�@�9X@˅@�-@���@Ǿw@Ƨ�@ũ�@�1'@��@��@��h@��@�Ĝ@�bN@��@���@���@���@�r�@���@�&�@��`@��y@�-@���@�%@��@��P@�v�@�V@��;@���@��@���@��-@��/@��@���@�(�@�  @�t�@���@���@�J@��@�Q�@�I�@� �@�S�@�t�@��@�\)@�
=@���@�@�@�V@���@��@���@���@�|�@�~�@���@�`B@��j@�Z@�  @��;@��@��!@�E�@��@�z�@�Q�@�Q�@� �@���@��
@���@�ƨ@���@�t�@�@�n�@�J@��@�V@��u@��u@�Q�@�  @���@�+@��y@��R@��!@�=q@���@���@��7@�&�@���@��D@�A�@���@��P@�;d@�@���@��\@��+@�v�@�^5@���@���@��7@�x�@�`B@��@��`@���@�A�@�1@���@�dZ@��@��R@���@���@��\@�~�@�ff@�M�@��@��-@�`B@�`B@�?}@�V@��`@�Ĝ@��D@�A�@�1@���@���@��P@�t�@�l�@�\)@�C�@�"�@�@���@��H@��@��@���@���@�v�@�=q@�$�@��#@���@�`B@��@�V@��`@�Ĝ@��9@���@�z�@�Z@�I�@���@���@��@��@�S�@�"�@��@���@�V@�E�@�5?@�$�@�@��^@��7@�hs@�G�@�7L@��@��@�Ĝ@�Ĝ@��j@��9@��u@�bN@�Z@�I�@�1'@�1@��m@��
@�|�@�S�@�;d@�33@�C�@�33@�33@�"�@���@��H@���@���@�V@�=q@�5?@�-@��@�J@�@�x�@�?}@�V@���@��@���@�I�@��m@���@���@�|�@��@�t�@�dZ@�;d@��@���@�v�@�V@�M�@�M�@�V@�M�@�5?@�$�@�{@�J@���@��T@��^@��h@��@�X@�7L@�%@���@�z�@�1@|�@l�@K�@+@�@~�y@~�+@}�@}�-@}�h@}p�@}�@|��@|j@|I�@{��@{��@{C�@z�H@zM�@yhs@x�`@xbN@w�w@w
=@v��@vȴ@w�@v�R@v�R@vE�@uV@tI�@t1@s��@s"�@r�H@r�H@rM�@r�@q�@q�^@q��@q�7@q%@pbN@o�@o;d@n�y@n��@m�T@m�@l9X@kt�@j~�@i��@i��@i�7@iX@i�@h��@h��@h��@hQ�@hb@g�w@g\)@g+@f�+@e@e��@e��@d�/@d1@cƨ@c��@cdZ@c"�@b�@b��@b��@b-@a�^@`��@` �@_�@_�;@_l�@^�y@^�R@^V@]�@]�-@]O�@\�D@\1@[ƨ@[dZ@["�@[@Z��@Z^5@Y�#@Y�7@Y7L@XĜ@XQ�@W�;@W+@V��@Vff@V5?@U��@U�@U`B@T��@T9X@S�
@SS�@R�!@RM�@R-@R�@RJ@Q�@Q�^@QG�@P��@P�u@PQ�@P �@O��@O|�@N��@N��@N@M�-@M`B@M�@MV@L�@L�j@Lz�@Lj@LZ@LZ@L(�@K��@K�
@Kt�@J��@I�7@IG�@I7L@I7L@I�@H�@HQ�@H1'@G�@G�w@G�@G�P@Gl�@GK�@G�@G�@F��@F�@F5?@E��@EO�@E�@E�@D�@D�j@D��@Dz�@Dj@D(�@C�F@C"�@B��@BJ@Ax�@A%@@�9@@Q�@@1'@@b@@  @@  @?l�@?�@>ȴ@>5?@=�@=��@=�h@=`B@=?}@=�@<��@<Z@;�F@;S�@;@:��@:^5@9��@9&�@8��@8�u@8Q�@8b@7�P@7+@7+@6�y@6V@6E�@6{@5�h@5O�@5�@4��@4��@4��@4I�@3��@3��@3��@3"�@2��@2-@1�@1��@0�`@0�u@0A�@0b@0  @/�w@/;d@.�y@.��@.v�@.v�@.v�@.v�@.V@-�-@-�@-V@,��@,z�@,9X@,(�@,1@+�m@+dZ@+"�@*�!@*~�@*�\@*��@*�!@*��@*^5@*�@)�@)�^@)�7@)&�@(�9@(bN@(  @'�w@'|�@'+@&ȴ@&��@&v�@&ff@&E�@&{@%�T@%�-@%�h@%�h@%`B@%�@$��@$�@$j@$(�@#��@#�m@#�
@#�F@#�@#"�@#@"�H@"�H@"��@"��@"~�@!��@!x�@!&�@ ��@ �@ r�@ r�@ bN@ Q�@ Q�@  �@�w@\)@K�@;d@+@
=@�y@ȴ@�+@ff@V@E�@5?@@�T@�-@p�@?}@/@�@V@��@��@�@�/@��@�@z�@(�@�
@�F@��@t�@C�@��@�!@�\@^5@�@��@�7@7L@%@��@�@bN@A�@ �@b@�;@�w@��@�P@�P@\)@
=@ȴ@��@$�@�T@@�@/@�/@�j@��@j@9X@(�@��@�m@�m@��@C�@"�@�!@^5@=q@-@�@J@�#@��@�7@�7@��@��@�7@x�@X@&�@%@��@Ĝ@�9@bN@�@�w@|�@K�@\)@|�@+@
=@�R@ff@V@V@V@V@E�@@�T@��@@�h@p�@O�@�@��@�/@�@�@j@9X@(�@1@�m@dZ@o@
�@
�@
�@
�@
�H@
�H@
��@
��@
-@	��@	�#@	��@	�^@	x�@	X@	�@��@�`@��@��@�9@�@r�@bN@A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AɋDA�=qA���AǾwA�|�A��A�ȴAƙ�A�n�A�?}A�$�A�bNA�VA�S�A���A�z�A�bA�
=A�ZA�
=A�v�A�ZA�A�A���A�`BA��hA�1A�A�1'A��PA�jA�ZA��A�=qA�VA��!A��/A�M�A��A�r�A���A�=qA��HA��/A���A���A�C�A���A�n�A�A�VA��A���A�A�dZA��
A�I�A��A���A�^5A��A���A��A�%A�;dA�Q�A���A��wA��A���A�Q�A���A�+A���A�=qA��A��`A��A��!A�A�A�v�A���A��A�z�A�bNA��;A�ZA���A�ZA��-A��A�JA��`A�n�A���A�9XA��\A��A���A��A��HA�/A�I�A�ZA��mA���A���A��A~1A{ƨAyAvĜAuO�Au/As�FAo�FAl��Aj�AhM�Ae��Ab=qA]�AZ�jAX-AWt�AV=qAS��AR�HARA�AQ�-AQ?}AP�HAN�!AK�AJJAI�AI�^AI/AG�mAFv�AEABbA@E�A?��A?��A?oA<�A:�yA:ffA:A�A8�!A7oA6ffA5��A4 �A2JA1K�A0��A01'A.��A-dZA+�TA*�A)ƨA(E�A'�-A'�A&�A%�FA$ZA#oA"9XA!XA ��A 1'AhsA�AjA��A�RA�A�Ax�A|�AS�A��Ap�AE�A�A �AdZA��A�jAv�A�#A��A��A�A�#AVA
5?A	��A�A�TA��A?}Az�A�A�Ar�A ��A �@��@��@��@���@��D@�9X@�dZ@��R@�ff@��@�33@��@�Q�@�F@�@�`B@��D@�1@@�v�@�bN@�R@�I�@��H@�p�@�bN@�@�E�@�p�@�I�@��y@�E�@�G�@ܓu@� �@��@�X@��
@��@�I�@��@�=q@���@Ѓ@�\)@�@�9X@˅@�-@���@Ǿw@Ƨ�@ũ�@�1'@��@��@��h@��@�Ĝ@�bN@��@���@���@���@�r�@���@�&�@��`@��y@�-@���@�%@��@��P@�v�@�V@��;@���@��@���@��-@��/@��@���@�(�@�  @�t�@���@���@�J@��@�Q�@�I�@� �@�S�@�t�@��@�\)@�
=@���@�@�@�V@���@��@���@���@�|�@�~�@���@�`B@��j@�Z@�  @��;@��@��!@�E�@��@�z�@�Q�@�Q�@� �@���@��
@���@�ƨ@���@�t�@�@�n�@�J@��@�V@��u@��u@�Q�@�  @���@�+@��y@��R@��!@�=q@���@���@��7@�&�@���@��D@�A�@���@��P@�;d@�@���@��\@��+@�v�@�^5@���@���@��7@�x�@�`B@��@��`@���@�A�@�1@���@�dZ@��@��R@���@���@��\@�~�@�ff@�M�@��@��-@�`B@�`B@�?}@�V@��`@�Ĝ@��D@�A�@�1@���@���@��P@�t�@�l�@�\)@�C�@�"�@�@���@��H@��@��@���@���@�v�@�=q@�$�@��#@���@�`B@��@�V@��`@�Ĝ@��9@���@�z�@�Z@�I�@���@���@��@��@�S�@�"�@��@���@�V@�E�@�5?@�$�@�@��^@��7@�hs@�G�@�7L@��@��@�Ĝ@�Ĝ@��j@��9@��u@�bN@�Z@�I�@�1'@�1@��m@��
@�|�@�S�@�;d@�33@�C�@�33@�33@�"�@���@��H@���@���@�V@�=q@�5?@�-@��@�J@�@�x�@�?}@�V@���@��@���@�I�@��m@���@���@�|�@��@�t�@�dZ@�;d@��@���@�v�@�V@�M�@�M�@�V@�M�@�5?@�$�@�{@�J@���@��T@��^@��h@��@�X@�7L@�%@���@�z�@�1@|�@l�@K�@+@�@~�y@~�+@}�@}�-@}�h@}p�@}�@|��@|j@|I�@{��@{��@{C�@z�H@zM�@yhs@x�`@xbN@w�w@w
=@v��@vȴ@w�@v�R@v�R@vE�@uV@tI�@t1@s��@s"�@r�H@r�H@rM�@r�@q�@q�^@q��@q�7@q%@pbN@o�@o;d@n�y@n��@m�T@m�@l9X@kt�@j~�@i��@i��@i�7@iX@i�@h��@h��@h��@hQ�@hb@g�w@g\)@g+@f�+@e@e��@e��@d�/@d1@cƨ@c��@cdZ@c"�@b�@b��@b��@b-@a�^@`��@` �@_�@_�;@_l�@^�y@^�R@^V@]�@]�-@]O�@\�D@\1@[ƨ@[dZ@["�@[@Z��@Z^5@Y�#@Y�7@Y7L@XĜ@XQ�@W�;@W+@V��@Vff@V5?@U��@U�@U`B@T��@T9X@S�
@SS�@R�!@RM�@R-@R�@RJ@Q�@Q�^@QG�@P��@P�u@PQ�@P �@O��@O|�@N��@N��@N@M�-@M`B@M�@MV@L�@L�j@Lz�@Lj@LZ@LZ@L(�@K��@K�
@Kt�@J��@I�7@IG�@I7L@I7L@I�@H�@HQ�@H1'@G�@G�w@G�@G�P@Gl�@GK�@G�@G�@F��@F�@F5?@E��@EO�@E�@E�@D�@D�j@D��@Dz�@Dj@D(�@C�F@C"�@B��@BJ@Ax�@A%@@�9@@Q�@@1'@@b@@  @@  @?l�@?�@>ȴ@>5?@=�@=��@=�h@=`B@=?}@=�@<��@<Z@;�F@;S�@;@:��@:^5@9��@9&�@8��@8�u@8Q�@8b@7�P@7+@7+@6�y@6V@6E�@6{@5�h@5O�@5�@4��@4��@4��@4I�@3��@3��@3��@3"�@2��@2-@1�@1��@0�`@0�u@0A�@0b@0  @/�w@/;d@.�y@.��@.v�@.v�@.v�@.v�@.V@-�-@-�@-V@,��@,z�@,9X@,(�@,1@+�m@+dZ@+"�@*�!@*~�@*�\@*��@*�!@*��@*^5@*�@)�@)�^@)�7@)&�@(�9@(bN@(  @'�w@'|�@'+@&ȴ@&��@&v�@&ff@&E�@&{@%�T@%�-@%�h@%�h@%`B@%�@$��@$�@$j@$(�@#��@#�m@#�
@#�F@#�@#"�@#@"�H@"�H@"��@"��@"~�@!��@!x�@!&�@ ��@ �@ r�@ r�@ bN@ Q�@ Q�@  �@�w@\)@K�@;d@+@
=@�y@ȴ@�+@ff@V@E�@5?@@�T@�-@p�@?}@/@�@V@��@��@�@�/@��@�@z�@(�@�
@�F@��@t�@C�@��@�!@�\@^5@�@��@�7@7L@%@��@�@bN@A�@ �@b@�;@�w@��@�P@�P@\)@
=@ȴ@��@$�@�T@@�@/@�/@�j@��@j@9X@(�@��@�m@�m@��@C�@"�@�!@^5@=q@-@�@J@�#@��@�7@�7@��@��@�7@x�@X@&�@%@��@Ĝ@�9@bN@�@�w@|�@K�@\)@|�@+@
=@�R@ff@V@V@V@V@E�@@�T@��@@�h@p�@O�@�@��@�/@�@�@j@9X@(�@1@�m@dZ@o@
�@
�@
�@
�@
�H@
�H@
��@
��@
-@	��@	�#@	��@	�^@	x�@	X@	�@��@�`@��@��@�9@�@r�@bN@A�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
_;B
^5B
\)B
[#B
[#B
XB
S�B
P�B
L�B
H�B
F�B
^5B
bNB
YB
_;B
�%B
�VB
�\B
��B
��B
�3B
�RB
ÖB
ǮB
��B
�ZB
�B
�B  BuB2-BS�BiyBy�B�B�uB�B�wB��B��B�ZB�BbB-BI�BT�BVBbNBw�B�B�B� B~�Bx�B�B� By�Bx�Bx�Bz�Bs�BXBB�B.BoB�B�wB{�B`BB}�B�jB�fB1B{B��B�B��B�B�#B�B��BŢB�?B��B�=B~�B]/B\)B6FB#�B<jBA�B@�B;dB0!B'�B�BB
�mB
�B
��B
ÖB
�FB
��B
�bB
e`B
P�B
C�B
/B
�B
VB	��B	�B	�B	�fB	��B	�!B	��B	�\B	{�B	aHB	;dB	�B	\B		7B	B��B��B�B�B�B�B�`B�BǮBȴBȴBǮBĜBĜB�dB��B�JB�\B��B�B�B��B��B��B�B�B��B��B��B�oB�PB�7B�PB�7B� B}�By�Bv�Bp�Bm�BjBhsBdZB_;BW
BR�BR�BR�BT�BT�BR�BN�BK�BH�BH�BE�BE�BE�BE�BE�BC�BA�B?}B?}B=qB<jB;dB:^B9XB:^B:^B9XB8RB8RB7LB6FB6FB5?B49B49B49B/B$�B!�B�B�B�B!�B%�B(�B(�B(�B)�B(�B'�B&�B"�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B"�B"�B"�B"�B!�B"�B$�B#�B'�B'�B)�B+B'�B)�B+B)�B+B-B.B.B/B1'B49B6FB:^B<jB=qBD�BW
B^5B]/B`BB`BB`BB]/B\)B\)B^5Bp�Bs�Bq�Bs�Bw�Bz�B�B�B�1B�+B�+B�7B�=B�7B�7B�\B�hB�{B��B��B��B��B��B��B��B�B�B�9B�FB�XBȴBɺB��B��B��B�
B�B�B�/B�5B�;B�NB�TB�yB�B�B�B�B��B��B	  B	%B	DB	PB	hB	{B	�B	�B	�B	�B	!�B	%�B	(�B	,B	-B	1'B	5?B	6FB	8RB	=qB	?}B	A�B	B�B	C�B	D�B	G�B	I�B	L�B	L�B	L�B	M�B	M�B	P�B	S�B	T�B	T�B	VB	XB	ZB	[#B	^5B	_;B	bNB	dZB	hsB	m�B	n�B	o�B	o�B	p�B	q�B	t�B	w�B	x�B	|�B	|�B	~�B	� B	�B	�B	�B	�1B	�7B	�JB	�\B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�9B	�9B	�9B	�?B	�?B	�FB	�XB	�jB	�qB	�wB	�}B	��B	B	B	ǮB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�5B	�5B	�5B	�BB	�HB	�HB	�HB	�TB	�TB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
%B
%B
%B
+B
+B
+B
+B
+B
1B
1B
+B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
DB
JB
JB
PB
PB
VB
VB
\B
hB
oB
hB
hB
oB
oB
oB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
,B
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
.B
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
49B
5?B
5?B
5?B
6FB
5?B
7LB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
T�B
VB
VB
VB
VB
W
B
W
B
W
B
VB
W
B
W
B
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
iyB
jB
jB
k�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
_B
^B
[�B
Z�B
Z�B
W�B
S�B
P�B
L�B
H�B
FtB
^B
bB
X�B
_B
��B
�"B
�(B
�~B
��B
��B
�B
�aB
�zB
бB
�&B
�KB
�iB
��B&B1�BS�BiDBy�B��B�&B��B�BB̘B̈́B�&B�OB.B,�BIlBT�BU�BbBw�B��B��B�B~�Bx�B��B�By�Bx�Bx�Bz�Bs�BW�BB[B-�B:B�iB�BB{�B_�B}�B�6B�2B�BFB�B�CB��B�iB��BյB̘B�mB��B��B��B~�B\�B[�B5�B#�B<6BA;B@4B;0B/�B'�BdB�B
�8B
��B
�~B
�GB
��B
��B
�.B
e,B
P�B
CaB
.�B
eB
"B	��B	�6B	�=B	�2B	�~B	��B	�WB	�(B	{�B	`�B	;0B	WB	(B	�B	�B��B�B�OB�cB�=B�6B�BյB�_B�fB�fB�_B�MB�gB�B�jB�B�B��B��B��B��B��B��B��B��B��B��B�pB� B�B��B�B��B�B}�By�BvzBpUBmCBj0Bh$BdB^�BV�BR�BR�BR�BT�BT�BR�BN�BKxBHfBHfBESBESBESBESBESBCGBA;B?.B?.B="B<B;B:B9	B9�B:B9	B8B8B6�B5�B5�B4�B3�B3�B3�B.�B$�B!|BpBIBdB!|B%�B(�B(�B(�B)�B(�B'�B&�B"hBWBQBKB+BBEBEBEB?B?BEB?B2B2B,B9BEBWBdBpBOBjBjBdBjBjB \B vB!|B"�B"�B"�B"�B!|B"�B$�B#nB'�B'�B)�B*�B'�B)�B*�B)�B*�B,�B-�B-�B.�B0�B3�B5�B9�B<B="BD3BV�B]�B\�B_�B_�B_�B\�B[�B[�B]�BpUBshBqABsMBw�BzxB��B��B��B��B��B��B��B��B��B�B�B�,B�KB�]B�pB�hB��B��B��B��B��B��B��B��B�fB�lB�^BЗBԯB֡B��B��B��B��B��B��B��B�B�=B�OB�GB�hB�lB��B��B	�B	
�B	B	 B	,B	$B	1B	dB	pB	!bB	%zB	(�B	+�B	,�B	0�B	4�B	5�B	7�B	="B	?.B	A;B	B'B	C-B	DMB	G_B	IRB	LdB	LdB	LdB	M�B	M�B	P�B	S�B	T�B	T�B	U�B	W�B	Y�B	Z�B	]�B	^�B	a�B	c�B	h
B	mCB	n/B	oOB	oOB	p;B	q[B	tnB	w�B	x�B	|�B	|�B	~�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�&B	�B	�B	�?B	�1B	�CB	�dB	�\B	�\B	�bB	�bB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�;B	�AB	�AB	�EB	�EB	�fB	�KB	�rB	�~B	�~B	̈́B	̈́B	�pB	�pB	�vB	ЗB	ЗB	�}B	�}B	ѝB	ңB	ңB	҉B	өB	ԯB	ԯB	՛B	ּB	֡B	֡B	��B	��B	خB	��B	ٴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	��B	�B	�B	�0B	�0B	�B	�B	�IB	�IB	�OB	�5B	�OB	�5B	�OB	�OB	�OB	�OB	�UB	�UB	�UB	�UB	�[B	�[B	�GB	�aB	�aB	�MB	�nB	�`B	�`B	�fB	�fB	��B	��B	��B	��B	��B	��B	�lB	��B	��B	��B	��B	��B	�xB	��B	�xB	��B	��B	�xB	�xB	�B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B

�B

�B

�B

�B

�B
�B
�B
B
B
B
�B
B
B
 B
B
B
 B
B
B
B
B
&B
B
2B
B
9B
9B
9B
?B
?B
?B
$B
?B
$B
?B
EB
?B
EB
EB
EB
EB
1B
7B
WB
]B
]B
IB
OB
OB
VB
VB
VB
 vB
 \B
!bB
!|B
!bB
"�B
"�B
#�B
$�B
$tB
$tB
$�B
$tB
$tB
$tB
%�B
%zB
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
2�B
2�B
3�B
3�B
4�B
4�B
4�B
5�B
4�B
6�B
6�B
6�B
6�B
6�B
8B
7�B
9	B
8�B
8�B
9	B
:B
;B
;B
;B
<B
<B
="B
="B
="B
>(B
>(B
>B
>(B
?.B
>(B
?B
?.B
?B
?B
?B
@4B
@4B
@4B
@4B
@4B
A;B
BAB
BAB
B'B
CGB
D3B
D3B
DMB
DMB
E9B
ESB
E9B
E9B
ESB
E9B
FYB
F?B
G_B
G_B
G_B
GEB
G_B
GEB
G_B
GEB
HKB
IlB
IlB
JrB
JrB
JXB
JrB
KxB
KxB
LdB
MjB
N�B
N�B
N�B
NpB
O�B
O�B
O�B
P}B
P}B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
T�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
U�B
V�B
V�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
^�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
a�B
a�B
cB
b�B
cB
cB
dB
c�B
c�B
dB
dB
c�B
dB
dB
eB
eB
e�B
fB
fB
fB
fB
fB
fB
gB
gB
h
B
h$B
i*B
iB
i*B
i*B
iB
jB
j0B
jB
jB
i*B
j0B
jB
kB
l=B
l=B
mCB
mCB
l"B
mCB
mCB
mCB
mCB
mCB
mCB
m)B
mCB
mCB
n/B
nIB
nIB
n/B
nIB
nIB
nIB
nIB
oOB
oOB
o5B
oOB
oOB
oOB
oOB
o5B
o5B
pUB
pUB
pUB
pUB
p;B
p;B
p;B
p;B
pUB
qAB
q[B
q[B
q[B
q[B
qAB
q[B
q[B
q[B
raB
raB
rGB
raB
ra111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.66(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201905210033552019052100335520190521003355201905220021102019052200211020190522002110JA  ARFMdecpA19c                                                                20190516153659  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190516063702  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190516063705  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190516063705  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190516063706  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190516063706  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190516063706  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190516063706  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190516063708  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190516063708                      G�O�G�O�G�O�                JA  ARUP                                                                        20190516065750                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190516153228  CV  JULD            G�O�G�O�F��A                JM  ARCAJMQC2.0                                                                 20190520153355  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190520153355  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190521152110  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120031518                      G�O�G�O�G�O�                