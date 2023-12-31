CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-28T00:35:19Z creation;2017-09-28T00:35:22Z conversion to V3.1;2019-12-19T08:00:36Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170928003519  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_163                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�)sǮ�1   @�)t{B_ @:�F
�L0�d�0��)1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBO��BW��B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBO��BW��B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�ffA�"�Aݺ^AݓuA݇+A�v�A�t�A�p�A�hsA�O�A���A܇+A۾wA�oA��A��A��Aں^Aڙ�A�z�A�K�A� �A��yA�1'A�1'A�"�A�Q�A�jA�ZA�n�A� �AÉ7A��yA��A���A���A�t�A���A�x�A��A�9XA��A�  A��\A� �A�;dA��HA��DA���A�O�A�G�A���A�C�A�n�A�ZA��A�9XA�l�A���A�v�A��^A���A��A��+A���A��yA�{A�ĜA�7LA��#A��hA�/A��A���A�  A�n�A� �A��A�XA���A�?}A�p�A�  A��9A�\)A�S�A���A�%A��!A�1'A�oA�M�A�&�A��7A�JA�^A~�`A~5?A}��A}�A{
=Ay�Ax�!Av�+Av{Au�AuƨAt�As��AsVAr��ArbNAq�Aq��AqS�Ap�AohsAnn�AmS�Alv�Ak�FAkAj1'Ai��AiO�Ah��AhM�Ag��Ag`BAf�Af(�AeƨAe33Ad��AdZAd �Ac\)Ab5?A`��A_"�A^Q�A^1'A^�A]�-A\�A[�PAYt�AXM�AW�TAW�TAW��AW�TAW�
AV��AVAT��AR�AR$�AP�RAN��AM�wAL��AK�AJVAI�AH�\AHJAG�#AG�AFffADr�ACO�ABjAA�FAAC�A@�9A?�;A>�A>��A>�A=7LA<�9A<jA<A�A;��A:^5A9�7A9+A8��A8�+A8VA8A6��A5��A4��A4=qA3��A3�A2�A2ZA2 �A2JA1
=A/�A/A.I�A-t�A+A*�A)�wA(��A(jA($�A(JA'��A&��A%;dA$^5A#\)A!�A �A�mAbNA�!A��A��AAz�AO�A^5AK�A�A%A��AJA�AM�A�AA`BA�yA��Az�A�hAn�A$�A  A�
A��AhsA&�A
��A
�A	��A��A^5AbAG�A�uAbNA�-A%A�An�AbNAE�A9XA1A��AJA"�A ��A M�@���@�hs@���@��D@���@�V@�$�@��T@���@�9X@�A�@�V@�j@��@�?}@��@�E�@��@�@�V@�dZ@�`B@�r�@�C�@�^5@���@�Z@۝�@�o@�J@�9X@�C�@���@�-@�G�@�r�@�o@ѡ�@�%@Ѓ@�+@�@���@̃@�9X@�l�@�"�@�v�@�r�@ǅ@��H@�~�@���@�?}@��^@�Z@�I�@�1'@�  @�|�@�ȴ@�$�@��@��j@��@���@�|�@��@��D@�1@�\)@��!@�{@�p�@���@���@�n�@�V@�@��7@�X@���@�1'@�C�@���@�n�@�=q@��^@�?}@���@�A�@��P@��@���@�$�@��h@�?}@��@��@�b@�l�@��H@��\@�5?@�{@���@�X@�V@���@�1@�ƨ@��@�l�@�S�@�"�@���@�v�@�M�@�{@��^@��@���@���@�dZ@���@�ff@���@�j@���@�S�@�33@��@���@�$�@�J@���@��#@�/@�1@�
=@��T@���@��9@��;@�"�@��\@�$�@���@��^@�x�@�?}@�V@���@��m@���@���@��@��H@�{@��T@��-@�p�@�/@���@��@�r�@�bN@�I�@�A�@�9X@�(�@� �@�1@��
@�;d@�^5@��@��^@��h@�p�@�7L@��j@�Z@��P@��!@�V@�V@�V@�=q@��T@���@���@�p�@�X@�?}@��@�%@���@���@��9@��D@�Q�@�  @��@�@~�+@}��@}V@|�@{�@{@z~�@y��@y%@xb@v�@v5?@u/@s�F@st�@sdZ@r�H@r�@q7L@pbN@pQ�@p1'@p �@o�;@o�P@o;d@n��@n�R@m�@m?}@m/@l�@lj@l1@j�@j-@i�7@i&�@h��@hĜ@hA�@h  @g�;@g��@g|�@gl�@g
=@fV@e�@e/@d�D@dZ@cƨ@b��@b��@bn�@a��@a��@aG�@`Ĝ@`r�@`1'@_|�@^��@^��@^$�@]�h@]�@\I�@[��@[ƨ@[t�@[C�@Z��@Z��@Z��@Z�!@Z�\@Y�#@Y�@Y%@X��@Xb@W��@W|�@WK�@WK�@W�@V�@V��@V5?@U@U�h@U`B@T�@TZ@T1@S��@St�@S33@R��@R-@Q�#@Q��@Q�7@Qx�@Q7L@P�9@PQ�@O�@Ol�@O;d@O;d@O;d@O
=@NV@N{@M��@M�-@M�@L��@L�j@L�@L�D@L�D@Lz�@LZ@K��@K�F@K��@KC�@K"�@J�H@J~�@JJ@I�@Ix�@H��@H��@H�u@HbN@HQ�@G�@G�@G�@Gl�@F��@FV@F@E��@E�h@E�h@E`B@E�@D��@Dz�@DZ@C��@C�
@C�F@C��@Ct�@B�@Bn�@B-@A��@Ax�@AG�@@�`@@�u@@bN@@  @?��@?��@?�P@?\)@?�@>��@>v�@>v�@>{@=@=�-@=�@<z�@<�@;ƨ@;33@:�H@:�H@:�H@:��@:��@:-@:J@9��@9�#@9��@8�`@8Ĝ@8��@8A�@8b@7�@7�@7|�@7+@7+@7
=@6�R@6�+@65?@5�@4��@4z�@49X@4�@3�@3C�@3"�@3@2�H@2�!@2n�@1��@1G�@1hs@1&�@0��@0bN@/�;@/��@/�w@/�w@/��@/�@.�@.v�@.5?@.$�@.$�@-��@-@-��@-�@,�/@,�D@,9X@+�
@+��@+��@+��@+��@+t�@+o@*��@*�!@*�\@*��@*�!@*�\@*~�@*M�@)�7@)7L@)%@(�9@(�u@(r�@(A�@'l�@&ȴ@&��@&�+@&v�@&V@&5?@&{@&@%@%`B@%V@$�D@$j@$I�@$9X@$1@#�F@#�@#C�@#"�@"�@"��@"^5@!��@!�#@!��@!x�@!%@ �9@ ��@ bN@  �@�@;d@�@�@5?@�@��@@�-@/@j@1@�m@�F@�@�@dZ@33@o@@�!@n�@^5@�@��@G�@��@Ĝ@�9@1'@�@l�@+@;d@
=@�R@�R@�+@ff@ff@E�@{@�-@/@V@�D@1@1@�@�m@��@dZ@�@�!@�\@^5@-@J@�#@��@x�@X@%@Ĝ@�9@�@Q�@A�@  @|�@\)@;d@+@�@ȴ@�R@�+@�+@v�@E�@E�@E�@E�@$�@��@�@V@�@�/@�/@�@�@�@��@��@�@�@��@��@Z@(�@1@��@C�@o@
�H@
�!@
�!@
�\@
n�@
J@	��@	��@	�@	�@	�@	�#@	�#@	��@	��@	�7@	X@	�@	�@	%@	%@�`@Ĝ@�@A�@r�@r�@bN@Q�@1'@�@�w@�w@�P@;d@ȴ@��@V@E�@5?@$�@$�@{@@�@�@�T@@�h@`B@�@�@�/@��@��@z�@Z@9X@�@�m@ƨ@��@��@��@S�@�H@��@�\@n�@^5@M�@=q@-@�@��@��@�^@��@��@��@��@x�@X@7L@&�@ ��@ ��@ ��@ Ĝ@ �9@ �u@ �@ �@ �@ bN@ Q�@ A�@ 1'?��w?�\)?�;d?�;d?��?���?�v�?�V?�V?�5??��?�p�?�V?��?��D?�j?��m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A�ffA�"�Aݺ^AݓuA݇+A�v�A�t�A�p�A�hsA�O�A���A܇+A۾wA�oA��A��A��Aں^Aڙ�A�z�A�K�A� �A��yA�1'A�1'A�"�A�Q�A�jA�ZA�n�A� �AÉ7A��yA��A���A���A�t�A���A�x�A��A�9XA��A�  A��\A� �A�;dA��HA��DA���A�O�A�G�A���A�C�A�n�A�ZA��A�9XA�l�A���A�v�A��^A���A��A��+A���A��yA�{A�ĜA�7LA��#A��hA�/A��A���A�  A�n�A� �A��A�XA���A�?}A�p�A�  A��9A�\)A�S�A���A�%A��!A�1'A�oA�M�A�&�A��7A�JA�^A~�`A~5?A}��A}�A{
=Ay�Ax�!Av�+Av{Au�AuƨAt�As��AsVAr��ArbNAq�Aq��AqS�Ap�AohsAnn�AmS�Alv�Ak�FAkAj1'Ai��AiO�Ah��AhM�Ag��Ag`BAf�Af(�AeƨAe33Ad��AdZAd �Ac\)Ab5?A`��A_"�A^Q�A^1'A^�A]�-A\�A[�PAYt�AXM�AW�TAW�TAW��AW�TAW�
AV��AVAT��AR�AR$�AP�RAN��AM�wAL��AK�AJVAI�AH�\AHJAG�#AG�AFffADr�ACO�ABjAA�FAAC�A@�9A?�;A>�A>��A>�A=7LA<�9A<jA<A�A;��A:^5A9�7A9+A8��A8�+A8VA8A6��A5��A4��A4=qA3��A3�A2�A2ZA2 �A2JA1
=A/�A/A.I�A-t�A+A*�A)�wA(��A(jA($�A(JA'��A&��A%;dA$^5A#\)A!�A �A�mAbNA�!A��A��AAz�AO�A^5AK�A�A%A��AJA�AM�A�AA`BA�yA��Az�A�hAn�A$�A  A�
A��AhsA&�A
��A
�A	��A��A^5AbAG�A�uAbNA�-A%A�An�AbNAE�A9XA1A��AJA"�A ��A M�@���@�hs@���@��D@���@�V@�$�@��T@���@�9X@�A�@�V@�j@��@�?}@��@�E�@��@�@�V@�dZ@�`B@�r�@�C�@�^5@���@�Z@۝�@�o@�J@�9X@�C�@���@�-@�G�@�r�@�o@ѡ�@�%@Ѓ@�+@�@���@̃@�9X@�l�@�"�@�v�@�r�@ǅ@��H@�~�@���@�?}@��^@�Z@�I�@�1'@�  @�|�@�ȴ@�$�@��@��j@��@���@�|�@��@��D@�1@�\)@��!@�{@�p�@���@���@�n�@�V@�@��7@�X@���@�1'@�C�@���@�n�@�=q@��^@�?}@���@�A�@��P@��@���@�$�@��h@�?}@��@��@�b@�l�@��H@��\@�5?@�{@���@�X@�V@���@�1@�ƨ@��@�l�@�S�@�"�@���@�v�@�M�@�{@��^@��@���@���@�dZ@���@�ff@���@�j@���@�S�@�33@��@���@�$�@�J@���@��#@�/@�1@�
=@��T@���@��9@��;@�"�@��\@�$�@���@��^@�x�@�?}@�V@���@��m@���@���@��@��H@�{@��T@��-@�p�@�/@���@��@�r�@�bN@�I�@�A�@�9X@�(�@� �@�1@��
@�;d@�^5@��@��^@��h@�p�@�7L@��j@�Z@��P@��!@�V@�V@�V@�=q@��T@���@���@�p�@�X@�?}@��@�%@���@���@��9@��D@�Q�@�  @��@�@~�+@}��@}V@|�@{�@{@z~�@y��@y%@xb@v�@v5?@u/@s�F@st�@sdZ@r�H@r�@q7L@pbN@pQ�@p1'@p �@o�;@o�P@o;d@n��@n�R@m�@m?}@m/@l�@lj@l1@j�@j-@i�7@i&�@h��@hĜ@hA�@h  @g�;@g��@g|�@gl�@g
=@fV@e�@e/@d�D@dZ@cƨ@b��@b��@bn�@a��@a��@aG�@`Ĝ@`r�@`1'@_|�@^��@^��@^$�@]�h@]�@\I�@[��@[ƨ@[t�@[C�@Z��@Z��@Z��@Z�!@Z�\@Y�#@Y�@Y%@X��@Xb@W��@W|�@WK�@WK�@W�@V�@V��@V5?@U@U�h@U`B@T�@TZ@T1@S��@St�@S33@R��@R-@Q�#@Q��@Q�7@Qx�@Q7L@P�9@PQ�@O�@Ol�@O;d@O;d@O;d@O
=@NV@N{@M��@M�-@M�@L��@L�j@L�@L�D@L�D@Lz�@LZ@K��@K�F@K��@KC�@K"�@J�H@J~�@JJ@I�@Ix�@H��@H��@H�u@HbN@HQ�@G�@G�@G�@Gl�@F��@FV@F@E��@E�h@E�h@E`B@E�@D��@Dz�@DZ@C��@C�
@C�F@C��@Ct�@B�@Bn�@B-@A��@Ax�@AG�@@�`@@�u@@bN@@  @?��@?��@?�P@?\)@?�@>��@>v�@>v�@>{@=@=�-@=�@<z�@<�@;ƨ@;33@:�H@:�H@:�H@:��@:��@:-@:J@9��@9�#@9��@8�`@8Ĝ@8��@8A�@8b@7�@7�@7|�@7+@7+@7
=@6�R@6�+@65?@5�@4��@4z�@49X@4�@3�@3C�@3"�@3@2�H@2�!@2n�@1��@1G�@1hs@1&�@0��@0bN@/�;@/��@/�w@/�w@/��@/�@.�@.v�@.5?@.$�@.$�@-��@-@-��@-�@,�/@,�D@,9X@+�
@+��@+��@+��@+��@+t�@+o@*��@*�!@*�\@*��@*�!@*�\@*~�@*M�@)�7@)7L@)%@(�9@(�u@(r�@(A�@'l�@&ȴ@&��@&�+@&v�@&V@&5?@&{@&@%@%`B@%V@$�D@$j@$I�@$9X@$1@#�F@#�@#C�@#"�@"�@"��@"^5@!��@!�#@!��@!x�@!%@ �9@ ��@ bN@  �@�@;d@�@�@5?@�@��@@�-@/@j@1@�m@�F@�@�@dZ@33@o@@�!@n�@^5@�@��@G�@��@Ĝ@�9@1'@�@l�@+@;d@
=@�R@�R@�+@ff@ff@E�@{@�-@/@V@�D@1@1@�@�m@��@dZ@�@�!@�\@^5@-@J@�#@��@x�@X@%@Ĝ@�9@�@Q�@A�@  @|�@\)@;d@+@�@ȴ@�R@�+@�+@v�@E�@E�@E�@E�@$�@��@�@V@�@�/@�/@�@�@�@��@��@�@�@��@��@Z@(�@1@��@C�@o@
�H@
�!@
�!@
�\@
n�@
J@	��@	��@	�@	�@	�@	�#@	�#@	��@	��@	�7@	X@	�@	�@	%@	%@�`@Ĝ@�@A�@r�@r�@bN@Q�@1'@�@�w@�w@�P@;d@ȴ@��@V@E�@5?@$�@$�@{@@�@�@�T@@�h@`B@�@�@�/@��@��@z�@Z@9X@�@�m@ƨ@��@��@��@S�@�H@��@�\@n�@^5@M�@=q@-@�@��@��@�^@��@��@��@��@x�@X@7L@&�@ ��@ ��@ ��@ Ĝ@ �9@ �u@ �@ �@ �@ bN@ Q�@ A�@ 1'?��w?�\)?�;d?�;d?��?���?�v�?�V?�V?�5??��?�p�?�V?��?��D?�j?��m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�!B��B�ZB��B%B%B+B%BBB  B  B	7B!�B;dBP�BVB\)Be`BjBq�Bq�Bm�BgmBXB)�B��B��B��B�BɺB�^BB�jB��B�?B�B�9B�qB�B��B�3B�!B��B�\Bv�BcTBm�B_;BC�B0!B�B�ZB�NB�)B�XB��B�'B��B��B��B�JB�DB�+Bx�B[#BM�BD�BD�BL�BJ�BG�B=qB?}B9XB33B/B(�B �BbB+B  B
��B
��B
�B
�`B
�/B
�
B
��B
��B
�wB
�!B
��B
��B
��B
��B
�{B
�\B
�JB
�+B
z�B
o�B
gmB
ZB
P�B
N�B
K�B
E�B
B�B
B�B
D�B
A�B
?}B
<jB
9XB
33B
-B
%�B
 �B
�B
�B
oB
VB
PB

=B
+B
B
B
  B	��B	��B	��B	�B	�B	�B	�B	�fB	�5B	�B	��B	ɺB	��B	ɺB	ƨB	�}B	�RB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�+B	�B	y�B	m�B	hsB	e`B	]/B	W
B	R�B	O�B	L�B	K�B	H�B	@�B	49B	2-B	0!B	,B	+B	&�B	!�B	�B	�B	�B	�B	�B	�B	uB	bB		7B	+B	+B	%B	B	B��B��B�B�B�B�B�yB�yB�mB�`B�NB�#B��B��B��B��BBB�jB�jB�qB�qB�jB�RB�B��B��B��B��B�hB�VB�%B~�B�B�B|�Bu�Bu�Bt�Bm�BiyBiyBjBhsBbNBdZBe`Be`BcTBbNBcTBaHB[#BZBcTBdZBdZBcTBbNB_;B]/B\)B\)B[#BZB\)BYBVBXBT�BP�BP�BO�BO�BN�BL�BI�B@�B>wB<jBA�B<jB8RB:^B<jB8RB)�B33B;dB8RB5?B-B �B)�B)�B)�B&�B-B-B2-B49B1'B-B+B0!B.B/B.B+B/B/B-B)�B.B1'B.B-B.B-B.B1'B1'B.B/B33B7LB9XB9XB:^B8RB5?B:^B=qB<jB6FB6FB:^BC�BK�BJ�BI�BH�BG�BG�BG�BH�BI�BL�BK�BI�BE�BP�BQ�BQ�BR�BQ�BQ�BXBaHBe`Be`BgmBiyBiyBiyBk�Bn�Bq�Bq�Bq�Bq�Br�Bt�Bt�Bw�Bx�Bw�By�B|�B|�B}�B� B�B�B�%B�+B�1B�1B�7B�DB�PB�JB�bB�oB�hB�oB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�'B�3B�LB�LB�LB�FB�RB�}BĜB��B��B��B�
B�/B�BB�TB�`B�`B�mB�sB�sB�sB�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	B	B	B	B	B	B	B	B	DB	VB	\B	hB	bB	bB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	!�B	#�B	#�B	$�B	%�B	%�B	&�B	&�B	(�B	+B	-B	/B	2-B	49B	7LB	8RB	9XB	:^B	<jB	=qB	<jB	=qB	?}B	B�B	D�B	H�B	H�B	N�B	T�B	T�B	T�B	T�B	W
B	[#B	bNB	cTB	dZB	cTB	e`B	hsB	k�B	l�B	l�B	o�B	r�B	r�B	s�B	t�B	t�B	x�B	{�B	|�B	~�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�=B	�PB	�PB	�VB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�-B	�'B	�-B	�9B	�FB	�LB	�LB	�RB	�RB	�^B	�^B	�dB	�jB	�qB	�qB	�}B	��B	B	B	ÖB	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�)B	�/B	�5B	�;B	�;B	�;B	�BB	�NB	�TB	�ZB	�fB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
+B
+B
%B
%B
+B
1B
	7B
	7B
	7B
DB
JB
JB
JB
PB
PB
VB
VB
VB
VB
VB
\B
bB
\B
bB
hB
oB
oB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
#�B
#�B
$�B
#�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
)�B
+B
)�B
)�B
)�B
)�B
,B
-B
-B
.B
.B
.B
-B
/B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
49B
49B
5?B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
;dB
<jB
<jB
;dB
=qB
>wB
>wB
=qB
=qB
=qB
?}B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
C�B
D�B
F�B
H�B
H�B
I�B
H�B
H�B
I�B
J�B
J�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
Q�B
P�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
W
B
VB
W
B
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
YB
YB
XB
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
]/B
]/B
\)B
\)B
]/B
^5B
^5B
_;B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
aHB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
e`B
e`B
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
gmB
hsB
hsB
hsB
hsB
hsB
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
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
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
p�B
p�B
p�B
q�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B�\B�B�B%B?B+B?BSB{B �B B
�B"�B;�BP�BV9B\xBe�Bj�Br-BraBo BkQB`B1'BoB��B�XB� B�}B��B�?B��BĜB��B��B��B��B��B��B��B��B��B�B{�Bi�Bq'Bc�BIB5%BWB�B��B�!B�wB�mB�hB��B��B�8B�bB��B�KB{B_BPBGEBE�BM�BK�BH�B?�B@�B:�B4nB/�B)�B"�BuB�B�B
��B
��B
��B
�B
��B
�_B
�B
�JB
�B
�GB
�kB
�-B
��B
�1B
�gB
�.B
��B
�KB
}qB
q'B
i*B
\)B
Q�B
O(B
LJB
F�B
C�B
C-B
EB
BB
@ B
<�B
9�B
4TB
.cB
'8B
"B
�B
sB
[B
BB
�B

�B
�B
�B
�B
 iB	��B	��B	�fB	�nB	�[B	� B	�B	�B	��B	��B	�}B	ʦB	��B	�	B	�zB	��B	��B	�cB	�B	�BB	��B	��B	�B	�TB	�B	��B	�[B	�7B	�MB	{�B	o�B	i�B	f�B	^�B	X�B	TB	Q B	M�B	L0B	IlB	B[B	6�B	3�B	1AB	,�B	+�B	'�B	"�B	�B	OB	xB	�B	9B	B	�B	hB	
�B	1B	�B	�B	�B	{B��B�^B�B�B�B�QB�0B��B�
B��B��BܒB��B��B�B�6BĜB��B��B��B��B��B��B�	B��B��B�2B�NB�yB�B��B�KB�B�SB�aB~wBw�Bw�BvBo5Bk6Bj�Bk6BiyBc�Be,Be�Be�Bc�Bb�Bc�Ba�B\xB[qBc�Bd�Bd�Bc�Bb�B_�B^B\�B\�B\)B[	B\�BZ7BV�BX�BU�BQ�BQhBP.BPBO(BMBJrBBB?�B=�BBB=�B9�B;0B=B9XB,�B4B;�B8�B5�B.}B#nB+QB+6B+6B($B-�B.B2�B4�B1�B.cB,WB0�B/B/�B.�B+�B/�B/�B-�B+6B.�B1�B.�B-�B.�B-�B.�B1�B1�B/B0B3�B7�B9�B9�B:�B9$B6�B;B>B=<B88B88B<jBDgBK�BJ�BJ	BI7BHKBHKBHKBIRBJ=BMBL0BJ�BG+BQNBR�BR�BS�BR�BSBX�Ba|Be�Be�Bg�Bi�Bi�BjBl=BoBq�Bq�BrBrBsBu%Bu?Bx8By$BxRBzDB}<B}<B~]B�iB��B�gB�tB�zB�KB��B��B��B��B��B�}B��B��B��B��B��B��B��B��B��B�
B��B�7B�)B�5B�-B�|B��B�QB�wB�[B�aB��B��B��B��B��B��B�$B�OB�mB�B̈́B҉BרBݘB��B�B�B�B�B�B��B�B��B�B�B�B�B��B�B�B�B�(B	 4B	AB	-B	3B	3B	3B	3B	3B	gB	aB	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	+B	�B	�B	�B	�B	 B	!�B	"B	#�B	#�B	$�B	%�B	%�B	'B	'B	)DB	+6B	-CB	/iB	2aB	4�B	7�B	8�B	9�B	:�B	<�B	=�B	<�B	=�B	?�B	CB	EB	IB	I7B	OBB	UB	UB	U2B	UMB	W�B	[qB	bNB	cnB	dtB	c�B	e�B	h�B	k�B	l�B	l�B	o�B	r�B	r�B	s�B	t�B	u%B	y$B	|B	}<B	B	B	B	�B	�'B	�'B	�-B	�-B	�GB	�aB	�?B	�zB	�rB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�&B	�
B	�$B	�B	�B	�=B	�B	�B	�5B	�5B	�iB	�iB	�GB	�[B	�aB	�TB	�`B	�fB	�fB	�lB	�lB	�xB	��B	��B	��B	��B	��B	��B	��B	ªB	ªB	��B	��B	żB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	� B	� B	�B	�B	�B	�B	�?B	�7B	�=B	�=B	�=B	�#B	�=B	�=B	�WB	�CB	�IB	�OB	�VB	�VB	�pB	�vB	�hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	�B	�B	�"B	�(B	�B	�(B
 B
 B
;B
[B
MB
MB
3B
YB
B
+B
EB
YB
YB
EB
KB
	RB
	RB
	lB
^B
dB
dB
dB
jB
jB
pB
pB
<B
pB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
#�B
#�B
$�B
$B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
'B
'�B
)�B
+B
*B
*B
*B
*KB
,"B
-)B
-)B
./B
./B
./B
-wB
/OB
1AB
2B
2GB
2aB
2GB
2GB
2GB
2GB
2aB
2aB
3hB
4TB
4TB
5ZB
4nB
5ZB
5?B
5tB
6`B
6`B
6`B
6`B
7fB
8lB
8lB
8lB
8�B
9rB
:^B
:xB
:xB
:�B
;�B
<jB
<�B
;B
=�B
>wB
>wB
=�B
=�B
=�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
C�B
D�B
F�B
H�B
H�B
I�B
H�B
H�B
I�B
J�B
J�B
L�B
L�B
M�B
M�B
M�B
M�B
NB
NB
N�B
O(B
OB
O�B
P�B
Q B
Q B
RB
QB
RB
R B
R�B
S&B
SB
T,B
TB
TB
TB
UB
UB
UB
UB
VB
W$B
VB
W$B
X+B
X+B
XB
X+B
X+B
X�B
Y1B
YB
YB
Y1B
YB
ZB
ZB
Y1B
Y1B
X+B
ZB
Z7B
[=B
[#B
[#B
[#B
[#B
\CB
\B
\CB
\)B
\CB
\CB
\CB
]/B
]/B
\]B
\CB
]IB
^OB
^OB
_VB
^jB
^OB
^OB
_;B
_!B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_VB
_VB
`BB
`'B
`BB
`\B
`\B
abB
bhB
cnB
dZB
dZB
dZB
dtB
d�B
dtB
dZB
dtB
cnB
c�B
ezB
ezB
fLB
gmB
gRB
gmB
gmB
gmB
gmB
gmB
gmB
g�B
g�B
h�B
h�B
hsB
hsB
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
iyB
iyB
i�B
i�B
j�B
kkB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
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
p�B
p�B
p�B
q�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710020034202017100200342020171002003420201806221231222018062212312220180622123122201804050426412018040504264120180405042641  JA  ARFMdecpA19c                                                                20170928093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170928003519  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170928003520  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170928003521  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170928003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170928003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170928003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170928003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170928003522  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170928003522                      G�O�G�O�G�O�                JA  ARUP                                                                        20170928005614                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170928153811  CV  JULD            G�O�G�O�F�K�                JM  ARCAJMQC2.0                                                                 20171001153420  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171001153420  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192641  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033122  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                