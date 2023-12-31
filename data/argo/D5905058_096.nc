CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-14T18:35:42Z creation;2018-10-14T18:35:45Z conversion to V3.1;2019-12-26T01:22:32Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181014183542  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               `A   JA  I2_0675_096                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؈�1M��1   @؈��9 @6��	k���c[Q���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh��Bo��Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D:��D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D}��D~� D  D� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�3D�@ Dʀ D��3D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�Ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @^{@�p�@�p�A
�RA*�RAJ�RAj�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bc{Bkz�BrG�BzG�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C �C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�H�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�D *�D ��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D	*�D	��D
*�D
��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D *�D ��D!*�D!��D"*�D"��D#*�D#��D$*�D$��D%*�D%��D&*�D&��D'*�D'��D(*�D(��D)*�D)��D**�D*��D+*�D+��D,*�D,��D-*�D-��D.*�D.��D/*�D/��D0*�D0��D1*�D1��D2*�D2��D3*�D3��D4*�D4��D5*�D5��D6*�D6��D7*�D7��D8*�D8��D9*�D9��D:*�D:��D;${D;��D<*�D<��D=*�D=��D>*�D>��D?*�D?��D@*�D@��DA*�DA��DB*�DB��DC*�DC��DD*�DD��DE*�DE��DF*�DF��DG*�DG��DH*�DH��DI*�DI��DJ*�DJ��DK*�DK��DL*�DL��DM*�DM��DN*�DN��DO*�DO��DP*�DP��DQ*�DQ��DR*�DR��DS*�DS��DT*�DT��DU*�DU��DV*�DV��DW*�DW��DX*�DX��DY*�DY��DZ*�DZ��D[*�D[��D\*�D\��D]*�D]��D^*�D^��D_*�D_��D`*�D`��Da*�Da��Db*�Db��Dc*�Dc��Dd*�Dd��De*�De��Df*�Df��Dg*�Dg��Dh*�Dh��Di*�Di��Dj*�Dj�HDk*�Dk��Dl*�Dl��Dm*�Dm��Dn*�Dn��Do*�Do��Dp*�Dp��Dq*�Dq��Dr*�Dr��Ds*�Ds��Dt*�Dt��Du*�Du��Dv*�Dv��Dw*�Dw��Dx*�Dx��Dy*�Dy��Dz*�Dz��D{*�D{��D|*�D|��D}*�D}��D~${D~��D*�D��D�qD�R=D��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�=D�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD�ؤD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�=D�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD��D�X�D��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��=D��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqDqD��qD�qD�UqDÕqD��qD�qD�UqDĕqD��qD�qD�UqDŕqD��qD�qD�UqDƕqD��qD�qD�UqDǕqD��qD�qD�UqDȕqD��qD�qD�UqDɕqD��qD��D�UqDʕqD�ؤD�qD�UqD˕qD��qD�qD�UqD̕qD��qD�qD�UqD͕qD��qD�qD�UqDΕqD��qD�qD�UqDϕqD��qD�qD�UqDЕqD��qD�qD�UqDѕqD��qD�qD�UqDҕqD��qD�qD�UqDӕqD��qD�qD�UqDԕqD��qD�qD�UqDՕqD��qD�qD�UqD֕qD��qD�qD�UqDוqD��qD�qD�UqDؕqD��qD�qD�UqDٕqD��qD�qD�UqDڕqD��qD�qD�UqDەqD��qD�qD�UqDܕqD��qD�qD�UqDݕqD��qD�qD�UqDޕqD��qD�qD�UqDߕqD��qD�qD�UqD��qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�[�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AЏ\AН�AХ�AХ�AХ�AС�AЙ�AГuAБhAЃA�z�A�r�A�l�A�ffA�bNA�\)A�ZA�ZA�VA�O�A�O�A�I�A�C�A�-A�1A���A���A���A�AʬA�%A��A���A��A�bNA�ȴA�ĜA�JA�x�A��9A��
A�A�C�A��A��A��wA�/A�Q�A���A�/A��-A�M�A�33A��/A��PA���A��RA�z�A�$�A�$�A�ƨA���A��A�33A���A��RA�S�A�l�A���A���A�r�A�ZA��A��jA�(�A�=qA�(�A�l�A�?}A�ZA�
=A���A���A��A���A��uA���A���A�`BA��HA�33A��A��wA�z�A�7LA��A�S�A��/A�=qA�VA�%A���A��/A�ZA��A�l�A��!A�ĜA�A��DA��wA��A��7A~~�A}�mA}l�A|�`A{�Ay�At�Aq�An��Ak\)AgVAe%Ad=qAbA�A`�jA^�jA]�
A]l�A\^5A[33AZ$�AWO�ATbNAP�\AOO�AM33AJ��AI�wAHA�AF1AD �ACt�AB�AAl�A?%A=7LA<-A;t�A:��A9C�A7�A6��A69XA5hsA4��A4Q�A4ȴA4v�A4��A3+A2jA1K�A/�TA,��A+�FA+�A*VA* �A(1'A&�\A%hsA$1'A"��A"1'A!�A ȴA&�A��A�mA1'A��A�PAx�A�yA\)A��A�A��A��A1'AXA�\AVA�hA33A��A�TA7LA
1A	C�A	A��AI�A�;AhsA��A��AƨAS�A�jAK�An�AE�A(�A(�A$�AO�@��@�%@�ȴ@��@�S�@��/@�@�C�@��@��@��@�j@�@��^@�A�@�v�@�G�@�j@�+@�J@��/@���@�V@�`B@�I�@�\)@��@�ȴ@�x�@� �@�|�@ڟ�@�`B@�o@�@Դ9@�b@щ7@��@���@�j@��@���@�;d@���@��@�bN@��m@�K�@�$�@ȋD@�C�@Ə\@���@�z�@þw@�"�@�{@�/@���@��
@�l�@�@���@�?}@��@�@���@�b@�33@�=q@��7@��@��@�K�@�~�@�@��#@��h@�bN@�t�@�
=@��!@�M�@�{@�@���@�V@��m@�l�@�ȴ@�$�@���@�7L@��@�(�@�"�@���@���@��u@�z�@�(�@��m@��@�A�@��@���@��u@�1'@���@���@�o@��@��y@��@��@��R@�-@�J@���@���@���@�1'@�  @���@��m@��;@��
@�"�@�n�@�V@��@���@�X@��/@�Q�@�ƨ@�\)@�;d@�S�@�l�@�V@�/@���@��u@��@�Z@�I�@�I�@�Z@�1'@��@���@�"�@��@��!@���@�~�@�v�@�n�@�n�@�^5@�5?@�-@��T@���@�G�@���@���@��@��@�(�@���@��P@�l�@�ƨ@�1@�ƨ@��P@��@�n�@�@��h@�/@��@�z�@�  @���@�S�@�33@�;d@��
@� �@���@��@���@�v�@�^5@�V@�^5@�V@�V@�^5@�^5@��+@��@��@�o@�
=@���@�V@��-@���@���@��@��`@���@��j@��j@��9@��@��D@�r�@�Q�@�(�@��
@��@���@���@�l�@�@�ȴ@��R@��R@���@�~�@�$�@���@��@��@��@��@��T@���@��7@�O�@�7L@�/@�/@��`@���@�Z@�I�@� �@���@�|�@�K�@�"�@�"�@��@���@��+@�^5@�=q@�@��#@���@��^@�O�@�V@��/@���@�Ĝ@��@�bN@�A�@��@���@�o@��@���@���@�v�@�-@�J@��@�@���@�hs@�G�@�7L@�/@��@��@���@�r�@�bN@�Q�@�9X@�  @�P@~�y@~�+@}��@}?}@}/@}V@|�/@|z�@{�m@{�@{dZ@z~�@z-@zJ@y�^@y�@w�w@v�@vE�@u�-@u?}@u?}@t��@tj@s��@s�
@t1@t1@s�@s@r�!@r^5@rJ@q��@qx�@q7L@q%@p�9@pb@oK�@n��@n5?@n{@n{@n@m`B@l��@l��@lI�@k�
@kdZ@k@j��@i��@i�@h�9@g�w@gl�@g\)@gK�@fv�@e��@e/@eV@d�@d9X@c��@c��@c��@cS�@bJ@a��@a��@`�`@`��@`r�@`bN@`A�@`  @_|�@_+@^V@^5?@]�h@]V@\�@\z�@\9X@[�m@[��@[dZ@["�@Z�!@Z^5@Y��@Y�@Y�7@Y7L@Y%@X�@X  @W�P@W
=@V�+@VV@V5?@V{@U��@U��@U�@T��@TZ@TI�@T(�@S��@S33@R�H@R�!@R^5@Q�@Q��@Q��@QG�@P��@PĜ@P�u@Pr�@P1'@O�;@O�w@O|�@O;d@N��@N�@N��@Nff@M�@M�@M`B@M/@L��@L�@LZ@K��@KdZ@K@Jn�@JM�@J-@I��@I7L@HĜ@H�u@H�@H �@G\)@Gl�@Gl�@G|�@G\)@F�R@FV@F@E�@E�-@Ep�@E?}@E?}@E?}@E?}@EV@D�j@Dz�@DI�@D9X@D�@C�
@C�@C"�@B�H@Bn�@A��@A7L@@Ĝ@@�@@Q�@@b@?�P@>��@>��@>�+@>E�@>@=@=p�@=V@<�@<�/@<�@<�j@<�@<�j@<��@<1@;dZ@;dZ@;"�@;"�@:�H@:~�@:M�@:-@9��@9�@9�7@97L@8��@8�@81'@7�@7�;@7�@7�P@7\)@7�@7
=@6�y@6ȴ@6�R@6��@6V@65?@6@5�@5�@5�T@5@5p�@5V@4�@4�j@4z�@4j@4j@4Z@49X@41@3t�@2n�@2�@1�@1��@1�^@1�^@1��@1X@1G�@1�@0�`@0�9@0�u@0Q�@0 �@/�w@/��@/l�@/;d@/+@.��@.�+@.ff@.ff@.ff@.{@-�T@-�h@-?}@-�@,�@,��@,��@,1@+�F@+dZ@+"�@*�@*�!@*-@)��@)x�@)G�@)�@(�`@(��@(bN@(  @'�w@'��@'\)@&�@&��@&@%�h@%�@$�@$�D@$z�@$j@$I�@$9X@$1@#ƨ@#��@#C�@#@"^5@"-@!�@!x�@!G�@ �`@ �@�@��@l�@��@�@��@V@$�@�T@��@�@p�@O�@?}@�@��@�@�/@�j@��@I�@��@ƨ@��@dZ@"�@@�!@=q@�@J@��@�@�^@��@��@7L@��@�@bN@bN@A�@ �@  @��@�P@\)@+@
=@��@@��@`B@/@��@��@��@j@��@33@"�@@�!@M�@�@�@�@��@�7@��@��@�@bN@A�@1'@1'@1'@�@�w@�@�P@\)@�@
=@�y@ȴ@�R@��@�+@E�@�T@��@`B@�/@��@�j@�j@�@�@�@��@z�@j@Z@Z@I�@�@ƨ@��@dZ@S�@@
��@
n�@
M�@
-@	�@	��@	��@	G�@	&�@	�@��@�u@r�@1'@b@�@�;@�@�@��@�P@;d@;d@;d@+@�@�y@�y@�y@�y@ȴ@�+@5?@�@��@��@�@p�@?}@�@�@j@Z@9X@��@�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AЏ\AН�AХ�AХ�AХ�AС�AЙ�AГuAБhAЃA�z�A�r�A�l�A�ffA�bNA�\)A�ZA�ZA�VA�O�A�O�A�I�A�C�A�-A�1A���A���A���A�AʬA�%A��A���A��A�bNA�ȴA�ĜA�JA�x�A��9A��
A�A�C�A��A��A��wA�/A�Q�A���A�/A��-A�M�A�33A��/A��PA���A��RA�z�A�$�A�$�A�ƨA���A��A�33A���A��RA�S�A�l�A���A���A�r�A�ZA��A��jA�(�A�=qA�(�A�l�A�?}A�ZA�
=A���A���A��A���A��uA���A���A�`BA��HA�33A��A��wA�z�A�7LA��A�S�A��/A�=qA�VA�%A���A��/A�ZA��A�l�A��!A�ĜA�A��DA��wA��A��7A~~�A}�mA}l�A|�`A{�Ay�At�Aq�An��Ak\)AgVAe%Ad=qAbA�A`�jA^�jA]�
A]l�A\^5A[33AZ$�AWO�ATbNAP�\AOO�AM33AJ��AI�wAHA�AF1AD �ACt�AB�AAl�A?%A=7LA<-A;t�A:��A9C�A7�A6��A69XA5hsA4��A4Q�A4ȴA4v�A4��A3+A2jA1K�A/�TA,��A+�FA+�A*VA* �A(1'A&�\A%hsA$1'A"��A"1'A!�A ȴA&�A��A�mA1'A��A�PAx�A�yA\)A��A�A��A��A1'AXA�\AVA�hA33A��A�TA7LA
1A	C�A	A��AI�A�;AhsA��A��AƨAS�A�jAK�An�AE�A(�A(�A$�AO�@��@�%@�ȴ@��@�S�@��/@�@�C�@��@��@��@�j@�@��^@�A�@�v�@�G�@�j@�+@�J@��/@���@�V@�`B@�I�@�\)@��@�ȴ@�x�@� �@�|�@ڟ�@�`B@�o@�@Դ9@�b@щ7@��@���@�j@��@���@�;d@���@��@�bN@��m@�K�@�$�@ȋD@�C�@Ə\@���@�z�@þw@�"�@�{@�/@���@��
@�l�@�@���@�?}@��@�@���@�b@�33@�=q@��7@��@��@�K�@�~�@�@��#@��h@�bN@�t�@�
=@��!@�M�@�{@�@���@�V@��m@�l�@�ȴ@�$�@���@�7L@��@�(�@�"�@���@���@��u@�z�@�(�@��m@��@�A�@��@���@��u@�1'@���@���@�o@��@��y@��@��@��R@�-@�J@���@���@���@�1'@�  @���@��m@��;@��
@�"�@�n�@�V@��@���@�X@��/@�Q�@�ƨ@�\)@�;d@�S�@�l�@�V@�/@���@��u@��@�Z@�I�@�I�@�Z@�1'@��@���@�"�@��@��!@���@�~�@�v�@�n�@�n�@�^5@�5?@�-@��T@���@�G�@���@���@��@��@�(�@���@��P@�l�@�ƨ@�1@�ƨ@��P@��@�n�@�@��h@�/@��@�z�@�  @���@�S�@�33@�;d@��
@� �@���@��@���@�v�@�^5@�V@�^5@�V@�V@�^5@�^5@��+@��@��@�o@�
=@���@�V@��-@���@���@��@��`@���@��j@��j@��9@��@��D@�r�@�Q�@�(�@��
@��@���@���@�l�@�@�ȴ@��R@��R@���@�~�@�$�@���@��@��@��@��@��T@���@��7@�O�@�7L@�/@�/@��`@���@�Z@�I�@� �@���@�|�@�K�@�"�@�"�@��@���@��+@�^5@�=q@�@��#@���@��^@�O�@�V@��/@���@�Ĝ@��@�bN@�A�@��@���@�o@��@���@���@�v�@�-@�J@��@�@���@�hs@�G�@�7L@�/@��@��@���@�r�@�bN@�Q�@�9X@�  @�P@~�y@~�+@}��@}?}@}/@}V@|�/@|z�@{�m@{�@{dZ@z~�@z-@zJ@y�^@y�@w�w@v�@vE�@u�-@u?}@u?}@t��@tj@s��@s�
@t1@t1@s�@s@r�!@r^5@rJ@q��@qx�@q7L@q%@p�9@pb@oK�@n��@n5?@n{@n{@n@m`B@l��@l��@lI�@k�
@kdZ@k@j��@i��@i�@h�9@g�w@gl�@g\)@gK�@fv�@e��@e/@eV@d�@d9X@c��@c��@c��@cS�@bJ@a��@a��@`�`@`��@`r�@`bN@`A�@`  @_|�@_+@^V@^5?@]�h@]V@\�@\z�@\9X@[�m@[��@[dZ@["�@Z�!@Z^5@Y��@Y�@Y�7@Y7L@Y%@X�@X  @W�P@W
=@V�+@VV@V5?@V{@U��@U��@U�@T��@TZ@TI�@T(�@S��@S33@R�H@R�!@R^5@Q�@Q��@Q��@QG�@P��@PĜ@P�u@Pr�@P1'@O�;@O�w@O|�@O;d@N��@N�@N��@Nff@M�@M�@M`B@M/@L��@L�@LZ@K��@KdZ@K@Jn�@JM�@J-@I��@I7L@HĜ@H�u@H�@H �@G\)@Gl�@Gl�@G|�@G\)@F�R@FV@F@E�@E�-@Ep�@E?}@E?}@E?}@E?}@EV@D�j@Dz�@DI�@D9X@D�@C�
@C�@C"�@B�H@Bn�@A��@A7L@@Ĝ@@�@@Q�@@b@?�P@>��@>��@>�+@>E�@>@=@=p�@=V@<�@<�/@<�@<�j@<�@<�j@<��@<1@;dZ@;dZ@;"�@;"�@:�H@:~�@:M�@:-@9��@9�@9�7@97L@8��@8�@81'@7�@7�;@7�@7�P@7\)@7�@7
=@6�y@6ȴ@6�R@6��@6V@65?@6@5�@5�@5�T@5@5p�@5V@4�@4�j@4z�@4j@4j@4Z@49X@41@3t�@2n�@2�@1�@1��@1�^@1�^@1��@1X@1G�@1�@0�`@0�9@0�u@0Q�@0 �@/�w@/��@/l�@/;d@/+@.��@.�+@.ff@.ff@.ff@.{@-�T@-�h@-?}@-�@,�@,��@,��@,1@+�F@+dZ@+"�@*�@*�!@*-@)��@)x�@)G�@)�@(�`@(��@(bN@(  @'�w@'��@'\)@&�@&��@&@%�h@%�@$�@$�D@$z�@$j@$I�@$9X@$1@#ƨ@#��@#C�@#@"^5@"-@!�@!x�@!G�@ �`@ �@�@��@l�@��@�@��@V@$�@�T@��@�@p�@O�@?}@�@��@�@�/@�j@��@I�@��@ƨ@��@dZ@"�@@�!@=q@�@J@��@�@�^@��@��@7L@��@�@bN@bN@A�@ �@  @��@�P@\)@+@
=@��@@��@`B@/@��@��@��@j@��@33@"�@@�!@M�@�@�@�@��@�7@��@��@�@bN@A�@1'@1'@1'@�@�w@�@�P@\)@�@
=@�y@ȴ@�R@��@�+@E�@�T@��@`B@�/@��@�j@�j@�@�@�@��@z�@j@Z@Z@I�@�@ƨ@��@dZ@S�@@
��@
n�@
M�@
-@	�@	��@	��@	G�@	&�@	�@��@�u@r�@1'@b@�@�;@�@�@��@�P@;d@;d@;d@+@�@�y@�y@�y@�y@ȴ@�+@5?@�@��@��@�@p�@?}@�@�@j@Z@9X@��@�
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B"�B!�B"�B"�B"�B"�B"�B#�B#�B$�B%�B%�B%�B$�B$�B$�B$�B$�B#�B#�B#�B#�B"�B �B�B�B�B�B�BF�By�B�B� B�+B�B� B�%B�B�B�7B�=B�+B�+B�%B�B�B�B�B� B� B~�B�VB�DB�DB�+B�B�PB��B��B��B��B��B��B��B��B��B��B��B�hB�\B�JB}�BjBdZB]/BM�BK�BC�B8RB33B$�B�B�B�B+BB��B�
B��BB�RB��Bu�BZB@�B"�B{BVBBBB%B%B	7B
�B
�B
ȴB
�^B
�B
��B
��B
�bB
�DB
x�B
t�B
p�B
k�B
aHB
S�B
,B
%B	�B	��B	��B	�uB	�PB	{�B	r�B	ffB	ffB	e`B	_;B	T�B	I�B	0!B	hB�B�yB�NB�B��BȴB�qB�?B�-B�!B�B��B��B�{B�hB�PB�7B~�B~�B�B�B�B�%B��B��B��B��B��B��B��B��B�PB�DB�1B�B�Bz�Bx�Br�Bo�Bm�Bk�BiyBffB`BB^5BYBW
BVBT�BS�BP�BL�BK�BI�BI�BG�BF�BC�BC�B=qB<jB:^B8RB6FB0!B1'B/B/B/B0!B0!B.B.B.B.B,B.B+B+B+B)�B(�B)�B%�B$�B#�B"�B �B!�B"�B"�B!�B!�B!�B$�B-B-B+B+B)�B)�B)�B'�B)�B)�B)�B)�B)�B(�B(�B'�B'�B(�B+B+B,B-B,B-B,B0!B0!B1'B33B33B33B49B33B2-B33B33B33B49B8RB=qB>wBD�BE�BH�BH�BM�BP�BR�BT�BVBW
BYBYB]/B_;B`BBbNBbNBcTBe`BgmBiyBl�Bp�Bu�Bv�Bx�B}�B�B�B�B�1B�7B�7B�7B�VB�uB��B��B��B��B��B��B��B�B�-B�RB�jB�}BBŢBȴB��B��B�B�NB�`B�fB�B��B	  B	B	B	%B	1B	1B	
=B	hB	uB	�B	�B	�B	�B	�B	�B	!�B	&�B	(�B	+B	/B	0!B	1'B	1'B	2-B	49B	:^B	=qB	A�B	D�B	F�B	I�B	K�B	M�B	N�B	O�B	Q�B	S�B	W
B	YB	]/B	`BB	aHB	bNB	dZB	ffB	gmB	hsB	hsB	hsB	iyB	k�B	l�B	o�B	p�B	s�B	v�B	x�B	y�B	y�B	{�B	|�B	�B	�B	�7B	�\B	�\B	�oB	�hB	�hB	�bB	�bB	�hB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�9B	�?B	�?B	�?B	�FB	�FB	�LB	�LB	�LB	�RB	�XB	�XB	�^B	�^B	�jB	�jB	�wB	�wB	�}B	��B	ÖB	ĜB	ŢB	ŢB	ƨB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�5B	�BB	�BB	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B

=B

=B

=B
DB
DB
JB
JB
PB
VB
VB
VB
VB
\B
\B
bB
hB
hB
bB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
-B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
0!B
0!B
0!B
0!B
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
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
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
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
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
T�B
T�B
T�B
VB
VB
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
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
ZB
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
^5B
^5B
^5B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
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
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
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
o�B
o�B
o�B
o�B
o�B
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
s�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B"�B!�B"�B"�B"�B"�B"�B#�B#�B$�B%�B%�B%�B$�B$�B$�B$�B$�B#�B#�B#�B#�B"�B �BxBqBkBxB�BFtBy�B��B�B��B��B�B��B��B��B�B�	B��B��B��B��B��B��B��B�B�B~�B�"B�B��B��B��B�B�MB��B��B��B��B��B��B�xB�WB�jB�QB�4B�(B��B}�Bj0Bd&B\�BM�BK�BCaB8B2�B$�BpBqB2B�B �B�B��B͟B�AB�B��ButBY�B@OB"�BFB"B�B�B�B�B�B	B
�vB
յB
ȀB
�B
��B
��B
�WB
�B
��B
x�B
t�B
poB
k6B
`�B
S�B
+�B
�B	�UB	ңB	��B	�&B	�B	{�B	raB	fB	f2B	eB	_B	T�B	IlB	/�B	B�[B�*B��BյBΊB�fB�"B��B��B��B��B�vB�]B�,B�B�B��B~�B~�B��B��B��B��B�]B�jB��B��B��B�|B�QB�9B�B��B��B��B��Bz�Bx�BraBoOBmCBk6Bi*BfB_�B]�BX�BV�BU�BT�BS�BP�BL~BKxBIlBIlBG_BFYBCGBCGB="B<B:B7�B5�B/�B0�B.�B.�B.�B/�B/�B-�B-�B-�B-�B+�B-�B*�B*�B*�B)�B(�B)�B%�B$�B#�B"�B vB!|B"�B"�B!|B!|B!|B$tB,�B,�B*�B*�B)�B)�B)�B'�B)�B)�B)�B)�B)�B(�B(�B'�B'�B(�B*�B*�B+�B,�B+�B,�B+�B/�B/�B0�B2�B2�B2�B3�B2�B1�B2�B2�B2�B3�B8B="B>(BD3BESBHfBHfBM�BP�BR�BT�BU�BV�BX�BX�B\�B^�B_�Ba�Ba�BcBd�BgBi*Bl=Bp;ButBvzBx�B}�B��B��B��B��B��B��B��B�B�B�B�+B�WB�dB�VB�vB�tB��B��B��B�B�.B�AB�SB�KB�jBԯB��B��B��B��B�OB��B��B	�B	�B	�B	�B	�B		�B	B	B	?B	1B	7B	WB	jB	VB	!bB	&�B	(�B	*�B	.�B	/�B	0�B	0�B	1�B	3�B	9�B	="B	A;B	DMB	FYB	IlB	K^B	M�B	N�B	O�B	Q�B	S�B	V�B	X�B	\�B	_�B	`�B	a�B	dB	fB	gB	h$B	h$B	h
B	i*B	k6B	l=B	oOB	pUB	shB	v`B	x�B	y�B	y�B	{B	|�B	��B	��B	��B	�B	��B	� B	�B	�B	��B	�B	� B	� B	�&B	�B	�B	�B	�2B	�$B	�CB	�pB	�vB	�pB	�\B	�|B	�|B	�hB	��B	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�	B	��B	��B	�B	�B	�B	�(B	�B	�B	�-B	�3B	�SB	�9B	�YB	�YB	�KB	�lB	�lB	�rB	�XB	�XB	�rB	�^B	�~B	̈́B	̈́B	�jB	�jB	ϑB	�}B	ѝB	ңB	҉B	өB	ԕB	՛B	՛B	յB	רB	��B	��B	��B	ںB	��B	ںB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�
B	�
B	�B	�0B	�6B	�B	�6B	�"B	�CB	�CB	�CB	�)B	�/B	�/B	�/B	�UB	�;B	�;B	�;B	�AB	�[B	�hB	�MB	�TB	�nB	�nB	�nB	�nB	�tB	�zB	�`B	�zB	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B

�B

�B
�B
�B
B
B
�B
B
B
�B
B
B
 B
B
�B
 B
 B
&B
&B
B
B
,B
B
,B
,B
2B
2B
B
$B
$B
EB
+B
+B
KB
KB
QB
QB
7B
=B
=B
WB
WB
WB
]B
]B
CB
]B
dB
IB
dB
IB
OB
jB
jB
 vB
!|B
"hB
"hB
"hB
#�B
#�B
#nB
#�B
#�B
$�B
%zB
%�B
%zB
%zB
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
0�B
/�B
/�B
/�B
/�B
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
3�B
3�B
3�B
3�B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5�B
5�B
6�B
6�B
7�B
7�B
7�B
7�B
8B
8B
9	B
9	B
9	B
9	B
:B
:B
9�B
:�B
:�B
;B
;B
;B
:�B
;B
:�B
<B
="B
=B
="B
="B
>(B
>B
>B
>(B
>(B
>(B
?.B
?.B
?B
@4B
@4B
@B
@4B
@4B
@4B
A;B
A B
A;B
A;B
A B
A;B
A B
B'B
BAB
BAB
BAB
BAB
B'B
BAB
CGB
CGB
C-B
C-B
CGB
C-B
C-B
CGB
CGB
CGB
D3B
ESB
E9B
E9B
E9B
ESB
ESB
ESB
ESB
ESB
FYB
FYB
FYB
FYB
FYB
FYB
FYB
F?B
G_B
G_B
G_B
G_B
G_B
G_B
G_B
GEB
HfB
HfB
IlB
IRB
IlB
JXB
JrB
JrB
JrB
JXB
JrB
JrB
JrB
JrB
JXB
K^B
K^B
KxB
KxB
KxB
LdB
L~B
M�B
M�B
M�B
M�B
NpB
NpB
NpB
O�B
O�B
P�B
P�B
P}B
P�B
P}B
P�B
P�B
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
T�B
T�B
T�B
U�B
U�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
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
Y�B
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
]�B
]�B
]�B
^�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
cB
b�B
cB
cB
c�B
c�B
c�B
c�B
c�B
dB
d�B
eB
eB
fB
e�B
fB
e�B
e�B
fB
fB
fB
e�B
fB
gB
gB
gB
gB
gB
gB
gB
gB
gB
h$B
h$B
h
B
i*B
iB
i*B
i*B
i*B
iB
iB
i*B
iB
j0B
jB
jB
j0B
j0B
jB
jB
j0B
k6B
kB
k6B
l=B
l=B
l"B
l=B
l=B
l"B
mCB
mCB
mCB
m)B
nIB
n/B
nIB
n/B
nIB
n/B
o5B
o5B
oOB
oOB
oOB
oOB
oOB
oOB
o5B
o5B
o5B
o5B
oOB
o5B
pUB
pUB
p;B
q[B
q[B
q[B
q[B
q[B
q[B
raB
raB
rGB
raB
sMB
sh11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.67(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810200037572018102000375720181020003757201810210026562018102100265620181021002656JA  ARFMdecpA19c                                                                20181015033515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181014183542  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181014183543  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181014183544  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181014183544  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181014183544  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181014183545  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181014183545  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181014183545  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181014183545                      G�O�G�O�G�O�                JA  ARUP                                                                        20181014185525                      G�O�G�O�G�O�                JA  ARUP                                                                        20191224081513                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181015153914  CV  JULD            G�O�G�O�F�GJ                JM  ARCAJMQC2.0                                                                 20181019153757  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181019153757  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181020152656  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                