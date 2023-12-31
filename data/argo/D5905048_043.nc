CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-29T21:35:14Z creation;2016-09-29T21:35:17Z conversion to V3.1;2019-12-19T08:25:22Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160929213514  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               +A   JA  I2_0577_043                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�β���1   @�γm�5 @3�|�����dɅ�Q�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�A�A�A�  A�A�A�A�A�A�%A�1A�1A�1A�1A�
=A�
=A�
=A�JA�
=A�JA�JA�JA�
=A�1A�
=A�{A�{A�{A��A��A��A��A��A��A��Aݟ�A�E�A��A���A���A�&�A�C�AЍPA�~�A�\)A�A�AΧ�A�x�A���A�S�A˺^A��;A�A��;A�(�A�I�A���A�bNAǰ!A�r�A�$�A�1A�9XA�\)A��A�E�A� �A�E�A�;dA���A��A�z�A��
A�A�A���A�oA�ƨA��A��RA�p�A��A�1A���A�hsA���A�^5A�
=A�7LA�A�A���A�|�A��9A���A��A�p�A�1'A��`A�A�A�dZA�9XA�-A��A��;A��yA��mA��A�\)A�^5A��A�+A�7LA��A�M�A�XA��wA&�A}"�Az��AwK�Au7LAql�ApAo��An�RAmK�Ak/AhJAg�FAe��Ab��A`�/A]�A\��AZVAW�hAT�jAR�AR5?APĜAN�9AMdZAL�AK`BAI�
AH��AG��AFZADVACƨAB��ABJAA
=A@�RA@v�A@(�A>�`A=�mA=C�A<�A:��A9��A8Q�A7oA5|�A3�A3%A2�RA1�
A0��A0�A/��A.��A-�-A+�A+x�A*~�A(��A'�-A&��A$z�A#%A!�TA ��A�;AhsA �A%A��A�HA-A�RA33Ax�AI�A��Ar�AQ�A��A;dAr�A�A9XA�TA�7A
��A	dZA	;dA��A��Az�A�A�7AM�A{A�TA��A\)A��AI�A?}An�A�A ��@��7@�dZ@�dZ@�K�@�l�@�Z@�K�@�V@�$�@��y@�Ĝ@�l�@�ȴ@��@�7@�t�@��@���@�h@�S�@��T@�z�@���@�33@�^5@��@߮@�@ޟ�@ݙ�@�ƨ@�X@�(�@��@���@�5?@�hs@�bN@��@щ7@�z�@�C�@��#@��/@��m@��y@Ɂ@�?}@�z�@���@��@Ƈ+@�=q@��@Ų-@ŉ7@��@���@�Z@+@�{@�x�@��@�%@��D@�j@�  @�+@��@�V@��`@��D@�j@�I�@���@�ff@���@�V@��/@��u@��@�p�@�hs@���@���@�O�@��+@�V@�J@���@���@��@��@�ȴ@���@�^5@�J@��T@��@��@��@�Q�@�Z@��m@���@�+@���@�@���@�M�@�@�G�@�V@��@��\@���@�?}@��j@�Ĝ@��;@�9X@�&�@�I�@�9X@�r�@��9@�Ĝ@�|�@���@�V@�J@�l�@��P@�|�@�C�@���@���@�O�@��9@��m@��;@��;@�1'@��@�$�@�?}@�%@���@�r�@�9X@���@���@�A�@��
@�K�@�n�@�-@�-@�{@�J@���@��u@�A�@��@��#@�p�@�x�@�x�@��@���@��@��w@���@���@�t�@��@���@���@�V@��-@�O�@�7L@�&�@��@� �@��@�(�@��@�K�@��@��R@��+@�$�@���@��^@�7L@��`@��j@��@�A�@�9X@�b@�t�@�K�@�33@�33@��@�l�@���@���@���@��P@��@��H@�=q@�$�@��@�J@��#@��^@���@��7@�/@��@��/@��@� �@�b@�  @�1@��@��m@��@�dZ@�\)@�S�@�C�@�"�@��y@���@��\@�M�@�J@���@���@��h@��@�x�@�p�@�X@�7L@���@�9X@��w@���@�t�@�\)@�;d@��@���@�=q@���@��@��j@�r�@���@�|�@�K�@�S�@�S�@�"�@���@�^5@�{@��h@�`B@�V@�Ĝ@�z�@�Q�@���@��@��
@�C�@��@�^5@�ff@�=q@���@�G�@�V@��@��D@�Q�@� �@��F@�t�@���@�=q@�J@��T@�@���@�hs@�/@��j@�bN@��@�P@K�@�@~�y@~�R@~v�@~@}�-@}�@|�@|9X@{�@z�H@z^5@y�@y�#@y�#@y%@xr�@w�;@v�y@v@u�@t�@t�D@tI�@t�@t�@s��@sdZ@s@r��@r��@r�@p�9@p  @o��@o
=@n$�@m�T@m@m�h@mO�@l�@l9X@l�@k��@k��@k33@j�@j��@jM�@i�#@ix�@h��@h��@h1'@g��@g�@g\)@g+@f��@f5?@e�-@d��@c��@c��@cC�@b~�@bJ@a��@ahs@a&�@`�`@`��@`A�@_�;@_;d@^ȴ@^�+@^5?@]�@\��@\��@\�@\j@[��@[33@Z��@Z�!@Z��@Z^5@Z�@Y�#@Y�^@Yx�@X��@X��@X�9@XA�@W��@W|�@W;d@V��@V$�@U�@U�T@Up�@T��@Tj@T�@S�m@S��@S��@SS�@S@R�H@R��@R=q@Q�^@Qx�@QX@Q&�@Q&�@P�9@P�u@P�@P1'@P  @O�@O�P@OK�@O+@Nȴ@NE�@N@N{@M�T@M�-@M��@M/@L��@Lz�@LI�@L(�@Kƨ@KdZ@KC�@J��@Jn�@I�@I�^@Ihs@IG�@I7L@I7L@I7L@I7L@I&�@HĜ@HbN@H1'@G�@G�w@G�P@F��@F�R@F��@Fv�@Fff@FV@FE�@F$�@E��@E/@D�D@D1@C�m@C�@CC�@CC�@B�@B�!@B��@B�\@Bn�@B�@BJ@BJ@A�@Ax�@AG�@@��@@��@@r�@@A�@@  @?�w@?|�@?K�@?�@>��@>V@>$�@=�@=p�@=?}@=/@<�/@<�@;�m@;�
@;ƨ@;��@;S�@:��@:~�@:J@9��@9�@8�`@8�@8A�@8b@7�@7�@7�w@7�P@7�@6�+@6E�@5@5�h@5�h@5`B@4�j@4�@3�
@3ƨ@3�F@3��@3��@3�@3t�@333@1��@1x�@1%@0Ĝ@0�@0bN@0Q�@0b@/�@/;d@.�@.��@.E�@.@-��@-p�@-?}@,�@,��@+��@+C�@+C�@+C�@+33@+o@*��@*=q@*�@*J@*J@*J@)�#@)x�@)hs@)X@)G�@)G�@)&�@)%@(��@(�u@(�@( �@'�@'�;@'�;@'��@'|�@'+@&��@&v�@&V@&E�@&5?@&$�@&@%�T@%@%�@$�D@#��@#�@"��@"�\@"n�@"-@!�@!�^@!��@!x�@!7L@ �`@ r�@ A�@ 1'@��@�w@��@|�@l�@K�@+@�@ff@E�@�@@��@�h@�h@O�@�@��@�/@z�@Z@�@ƨ@S�@33@�@=q@J@��@��@�#@x�@&�@�`@Ĝ@Q�@b@��@��@�P@K�@��@�y@�@��@ff@5?@�T@p�@O�@/@�@V@��@�/@�j@�D@z�@z�@j@Z@I�@9X@(�@�@�
@�@C�@�@��@��@��@�\@^5@=q@�@��@�@�@��@�^@�^@�^@�^@�^@��@�7@hs@X@G�@7L@7L@&�@�@%@%@��@��@Ĝ@��@�@bN@Q�@ �@�@��@�@�P@K�@K�@�@v�@E�@5?@{@�T@@�-@�-@��@��@�h@�@�@�@p�@p�@p�@/@��@�/@�j@�j@�j@�j@�j@�j@�j@�D@Z@9X@(�@�@�m@ƨ@��@�@t�@33@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�A�A�A�  A�A�A�A�A�A�%A�1A�1A�1A�1A�
=A�
=A�
=A�JA�
=A�JA�JA�JA�
=A�1A�
=A�{A�{A�{A��A��A��A��A��A��A��Aݟ�A�E�A��A���A���A�&�A�C�AЍPA�~�A�\)A�A�AΧ�A�x�A���A�S�A˺^A��;A�A��;A�(�A�I�A���A�bNAǰ!A�r�A�$�A�1A�9XA�\)A��A�E�A� �A�E�A�;dA���A��A�z�A��
A�A�A���A�oA�ƨA��A��RA�p�A��A�1A���A�hsA���A�^5A�
=A�7LA�A�A���A�|�A��9A���A��A�p�A�1'A��`A�A�A�dZA�9XA�-A��A��;A��yA��mA��A�\)A�^5A��A�+A�7LA��A�M�A�XA��wA&�A}"�Az��AwK�Au7LAql�ApAo��An�RAmK�Ak/AhJAg�FAe��Ab��A`�/A]�A\��AZVAW�hAT�jAR�AR5?APĜAN�9AMdZAL�AK`BAI�
AH��AG��AFZADVACƨAB��ABJAA
=A@�RA@v�A@(�A>�`A=�mA=C�A<�A:��A9��A8Q�A7oA5|�A3�A3%A2�RA1�
A0��A0�A/��A.��A-�-A+�A+x�A*~�A(��A'�-A&��A$z�A#%A!�TA ��A�;AhsA �A%A��A�HA-A�RA33Ax�AI�A��Ar�AQ�A��A;dAr�A�A9XA�TA�7A
��A	dZA	;dA��A��Az�A�A�7AM�A{A�TA��A\)A��AI�A?}An�A�A ��@��7@�dZ@�dZ@�K�@�l�@�Z@�K�@�V@�$�@��y@�Ĝ@�l�@�ȴ@��@�7@�t�@��@���@�h@�S�@��T@�z�@���@�33@�^5@��@߮@�@ޟ�@ݙ�@�ƨ@�X@�(�@��@���@�5?@�hs@�bN@��@щ7@�z�@�C�@��#@��/@��m@��y@Ɂ@�?}@�z�@���@��@Ƈ+@�=q@��@Ų-@ŉ7@��@���@�Z@+@�{@�x�@��@�%@��D@�j@�  @�+@��@�V@��`@��D@�j@�I�@���@�ff@���@�V@��/@��u@��@�p�@�hs@���@���@�O�@��+@�V@�J@���@���@��@��@�ȴ@���@�^5@�J@��T@��@��@��@�Q�@�Z@��m@���@�+@���@�@���@�M�@�@�G�@�V@��@��\@���@�?}@��j@�Ĝ@��;@�9X@�&�@�I�@�9X@�r�@��9@�Ĝ@�|�@���@�V@�J@�l�@��P@�|�@�C�@���@���@�O�@��9@��m@��;@��;@�1'@��@�$�@�?}@�%@���@�r�@�9X@���@���@�A�@��
@�K�@�n�@�-@�-@�{@�J@���@��u@�A�@��@��#@�p�@�x�@�x�@��@���@��@��w@���@���@�t�@��@���@���@�V@��-@�O�@�7L@�&�@��@� �@��@�(�@��@�K�@��@��R@��+@�$�@���@��^@�7L@��`@��j@��@�A�@�9X@�b@�t�@�K�@�33@�33@��@�l�@���@���@���@��P@��@��H@�=q@�$�@��@�J@��#@��^@���@��7@�/@��@��/@��@� �@�b@�  @�1@��@��m@��@�dZ@�\)@�S�@�C�@�"�@��y@���@��\@�M�@�J@���@���@��h@��@�x�@�p�@�X@�7L@���@�9X@��w@���@�t�@�\)@�;d@��@���@�=q@���@��@��j@�r�@���@�|�@�K�@�S�@�S�@�"�@���@�^5@�{@��h@�`B@�V@�Ĝ@�z�@�Q�@���@��@��
@�C�@��@�^5@�ff@�=q@���@�G�@�V@��@��D@�Q�@� �@��F@�t�@���@�=q@�J@��T@�@���@�hs@�/@��j@�bN@��@�P@K�@�@~�y@~�R@~v�@~@}�-@}�@|�@|9X@{�@z�H@z^5@y�@y�#@y�#@y%@xr�@w�;@v�y@v@u�@t�@t�D@tI�@t�@t�@s��@sdZ@s@r��@r��@r�@p�9@p  @o��@o
=@n$�@m�T@m@m�h@mO�@l�@l9X@l�@k��@k��@k33@j�@j��@jM�@i�#@ix�@h��@h��@h1'@g��@g�@g\)@g+@f��@f5?@e�-@d��@c��@c��@cC�@b~�@bJ@a��@ahs@a&�@`�`@`��@`A�@_�;@_;d@^ȴ@^�+@^5?@]�@\��@\��@\�@\j@[��@[33@Z��@Z�!@Z��@Z^5@Z�@Y�#@Y�^@Yx�@X��@X��@X�9@XA�@W��@W|�@W;d@V��@V$�@U�@U�T@Up�@T��@Tj@T�@S�m@S��@S��@SS�@S@R�H@R��@R=q@Q�^@Qx�@QX@Q&�@Q&�@P�9@P�u@P�@P1'@P  @O�@O�P@OK�@O+@Nȴ@NE�@N@N{@M�T@M�-@M��@M/@L��@Lz�@LI�@L(�@Kƨ@KdZ@KC�@J��@Jn�@I�@I�^@Ihs@IG�@I7L@I7L@I7L@I7L@I&�@HĜ@HbN@H1'@G�@G�w@G�P@F��@F�R@F��@Fv�@Fff@FV@FE�@F$�@E��@E/@D�D@D1@C�m@C�@CC�@CC�@B�@B�!@B��@B�\@Bn�@B�@BJ@BJ@A�@Ax�@AG�@@��@@��@@r�@@A�@@  @?�w@?|�@?K�@?�@>��@>V@>$�@=�@=p�@=?}@=/@<�/@<�@;�m@;�
@;ƨ@;��@;S�@:��@:~�@:J@9��@9�@8�`@8�@8A�@8b@7�@7�@7�w@7�P@7�@6�+@6E�@5@5�h@5�h@5`B@4�j@4�@3�
@3ƨ@3�F@3��@3��@3�@3t�@333@1��@1x�@1%@0Ĝ@0�@0bN@0Q�@0b@/�@/;d@.�@.��@.E�@.@-��@-p�@-?}@,�@,��@+��@+C�@+C�@+C�@+33@+o@*��@*=q@*�@*J@*J@*J@)�#@)x�@)hs@)X@)G�@)G�@)&�@)%@(��@(�u@(�@( �@'�@'�;@'�;@'��@'|�@'+@&��@&v�@&V@&E�@&5?@&$�@&@%�T@%@%�@$�D@#��@#�@"��@"�\@"n�@"-@!�@!�^@!��@!x�@!7L@ �`@ r�@ A�@ 1'@��@�w@��@|�@l�@K�@+@�@ff@E�@�@@��@�h@�h@O�@�@��@�/@z�@Z@�@ƨ@S�@33@�@=q@J@��@��@�#@x�@&�@�`@Ĝ@Q�@b@��@��@�P@K�@��@�y@�@��@ff@5?@�T@p�@O�@/@�@V@��@�/@�j@�D@z�@z�@j@Z@I�@9X@(�@�@�
@�@C�@�@��@��@��@�\@^5@=q@�@��@�@�@��@�^@�^@�^@�^@�^@��@�7@hs@X@G�@7L@7L@&�@�@%@%@��@��@Ĝ@��@�@bN@Q�@ �@�@��@�@�P@K�@K�@�@v�@E�@5?@{@�T@@�-@�-@��@��@�h@�@�@�@p�@p�@p�@/@��@�/@�j@�j@�j@�j@�j@�j@�j@�D@Z@9X@(�@�@�m@ƨ@��@�@t�@33@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B'�B'�B(�B(�B(�B'�B'�B'�B'�B'�B'�B'�B'�B'�B'�B(�B(�B(�B(�B'�B'�B(�B'�B'�B'�B'�B(�B(�B(�B(�B(�B(�B(�B(�B'�B$�B  B
ŢB
��B
�#B
�sBPB'�BbB�B1'B33B.B2-BhsB�B�'BDB,BQ�Be`Bw�B}�B�bB��B�B�}B��B��B��B�BiyB^5BjBjBo�Bm�Bq�Bk�BgmBgmB[#BJ�B>wB5?B&�B!�B�B�B�BuB"�B'�B�B��B�B�B�`B�NB�B��B��B�B�TB�;B��B��B��B�}B�B�DB[#B<jB+BoB
�B
��B
�FB
��B
��B
�bB
�B
q�B
`BB
J�B
<jB
)�B
�B
�B
oB
	7B	��B	�sB	�ZB	�B	ƨB	�dB	��B	��B	�\B	|�B	jB	_;B	YB	P�B	F�B	?}B	7LB	33B	,B	&�B	 �B	�B	bB	JB	1B	  B��B��B��B��B��B�B�B�`B�;B�B�B��B��BĜB��B�wB�wB�qB�jB�^B�RB�FB�9B�9B�FB�!B��B��B��B��B�uB�bB�VB�JB�DB�1B�1B�7B�JB�JB�7B�=B�7B�1B�By�Bw�Bv�Bo�Bn�Bk�Bk�Bk�Bm�Bm�Bl�Bl�Bl�Bl�Bk�BjBk�Bk�Bl�Bm�Bn�Bp�Bq�Bq�Bo�Bm�Bo�Bq�Bt�Bw�Bx�B�B��B��B��B�1B|�By�Bt�Bt�Bt�B� B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�3B�LB�qBĜBȴBɺB��B��B��B�B�#B�;B�TB�`B�sB�sB�mB�mB�B�B�B�B�B��B��B��B��B	  B	B		7B	JB	\B	oB	{B	�B	�B	�B	 �B	"�B	"�B	%�B	$�B	$�B	#�B	(�B	.B	49B	7LB	9XB	=qB	B�B	G�B	L�B	Q�B	R�B	VB	^5B	bNB	bNB	bNB	aHB	aHB	cTB	cTB	gmB	gmB	hsB	jB	s�B	z�B	}�B	�B	�+B	�=B	�DB	�DB	�JB	�=B	�B	� B	|�B	}�B	}�B	}�B	�7B	�bB	�DB	�=B	�PB	�JB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�3B	�9B	�RB	�RB	�RB	�LB	�?B	�FB	�FB	�wB	�dB	�LB	�?B	�?B	�FB	�LB	�RB	�RB	�^B	��B	��B	ÖB	B	��B	ÖB	ĜB	ĜB	ÖB	��B	��B	��B	�qB	�wB	��B	��B	ĜB	ĜB	ĜB	ƨB	ƨB	ƨB	ƨB	ȴB	ȴB	ɺB	ɺB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�5B	�NB	�sB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
	7B
	7B
	7B
	7B

=B
1B
1B
1B

=B

=B

=B
PB
PB
PB
PB
VB
\B
bB
hB
hB
oB
oB
�B
{B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
-B
-B
-B
-B
.B
.B
.B
.B
/B
/B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
6FB
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
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
@�B
@�B
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
B�B
B�B
B�B
C�B
B�B
C�B
C�B
C�B
C�B
D�B
C�B
D�B
C�B
D�B
D�B
E�B
E�B
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
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
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
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
W
B
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
YB
YB
YB
YB
YB
YB
YB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
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
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
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
ffB
ffB
ffB
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
jB
jB
jB
k�B
k�B
k�B
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
n�B
m�B
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
p�B
p�B
p�B
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
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B'�B'�B(�B(�B(�B'�B'�B'�B'�B'�B'�B'�B'�B'�B'�B(�B(�B(�B(�B'�B'�B(�B'�B'�B'�B'�B(�B(�B(�B(�B(�B)B)*B)�B*KB,=BfB
��B
�B
�!B
�B�B(�B�B�B2�B4TB.�B4Bj�B�AB�[B�B-�BS[BgBx�BcB��B��B�B�AB��B��B�1B��Bn/Bb�Bn}BmwBr�Bq�BsMBm�Bk6Bk6B_pBMBA;B7�B(�B"�B�BCB�B?B%�B*B�BUB�|B�qB�mB�,B��B �B�rB�=B�zB�HBԯBөBοBªB�!B�NB^�B?�B/5B_B
�B
�}B
�rB
�$B
�yB
�@B
��B
uB
dB
M�B
@4B
+�B
�B
B
�B
B
�B	��B	�8B	�~B	ɆB	��B	��B	��B	��B	�B	l�B	`�B	[#B	S@B	HfB	AB	8�B	5B	-wB	(�B	"�B	�B	hB	�B		7B	 B�wB�wB��B��B�$B��B�/B�B��B�B��B��B̘BżB�;B��B��B�]B�VB��B��B�8B�?B��B�lB��B��B�B��B�$B��B��B�\B�B��B��B��B��B�pB�pB�^B��B�rB�#B�tBz�Bx�BxBqABo�Bl=BlWBl�Bn�Bm�Bl�Bl�Bm)BmCBl�Bk�Bk�Bk�BmBn/Bo�Bq�BsBr�Bp�Bo�Bq�Br�Bt�Bw�Bx�B�B�SB��B�=B�=B~]Bz�BuZBu�BtnB}B�KB��B��B�5B��B��B�fB��B��B��B��B��B�wB�B��B��B�B�(B�B�7B�rBˬB��B�B��B�B�BB�B�2B�DB�DB��B�
B�B�B�B��B�B��B�B�>B�6B	 �B	B		�B	�B	�B	�B	�B	�B	)B	 vB	!�B	#TB	#B	&B	%B	%,B	$�B	)�B	.�B	4�B	7�B	9rB	=<B	BuB	G�B	MB	Q�B	R�B	U�B	^jB	b�B	b�B	b�B	a�B	a�B	c�B	cnB	g�B	g�B	hsB	i�B	s3B	z�B	}�B	�3B	�zB	��B	��B	��B	�B	�)B	��B	�iB	}<B	}�B	}�B	}qB	�RB	�4B	��B	�rB	��B	�B	�B	�B	��B	��B	��B	�B	��B	�ZB	�B	��B	�@B	� B	�GB	��B	��B	��B	��B	��B	��B	�tB	�`B	�zB	�HB	��B	��B	�tB	�tB	��B	��B	��B	�8B	�DB	��B	�B	�B	��B	��B	ðB	��B	�B	�MB	��B	�'B	��B	��B	�wB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�	B	�=B	�B	��B	��B	�)B	�B	��B	��B	�<B	�B	�:B	�&B	�,B	�MB	�2B	�9B	�SB	�2B	�,B	�,B	�9B	�$B	�EB	�B	�CB	�IB	�OB	�4B	�sB	�yB	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�"B	�(B	�B
 4B
 4B	��B	�B	��B	�.B
'B
GB
GB
-B
-B
-B
3B
MB
SB
YB
YB
_B
fB
KB
KB
	RB
	lB
	RB
	RB
	lB
	�B
	�B

�B

XB

XB

XB

XB

rB

�B

�B
	�B
	�B
	lB
	�B

�B
KB
KB
1B

XB

rB

�B
�B
�B
�B
�B
�B
vB
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
�B
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
�B
�B
!B
 �B
 �B
 �B
!�B
!�B
"B
"B
!�B
"4B
"B
# B
#B
"�B
"�B
#�B
#�B
$B
$B
$�B
$�B
%B
&2B
&fB
'B
'B
($B
)_B
)B
(�B
*B
*B
*0B
+6B
+B
+B
,=B
-)B
-)B
-)B
-)B
-)B
.cB
.IB
./B
.IB
/5B
/5B
/5B
/5B
/OB
/OB
0UB
0�B
1[B
1[B
1[B
1vB
2aB
2GB
2GB
2GB
2aB
2GB
2aB
2GB
1[B
1[B
1AB
1[B
2aB
2aB
33B
33B
3MB
3MB
3�B
4TB
5ZB
5ZB
5ZB
5ZB
5ZB
6`B
5ZB
6zB
6`B
6FB
6zB
6zB
7fB
7fB
7�B
8�B
8lB
8lB
8�B
9�B
9�B
:�B
:xB
:xB
:xB
:xB
:xB
;B
;B
;�B
;�B
;dB
<�B
<�B
<�B
<�B
<�B
<�B
<jB
=�B
=�B
=�B
=qB
>�B
>�B
>�B
?�B
?}B
?�B
?�B
?�B
?�B
?�B
@�B
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
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
C�B
C�B
C�B
C�B
D�B
C�B
D�B
C�B
D�B
D�B
E�B
E�B
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
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
MB
M�B
M�B
M�B
M�B
NB
N"B
N�B
N�B
OB
P.B
O�B
PB
Q B
Q B
Q B
P�B
Q B
Q B
R B
R B
SB
S&B
SB
TB
TB
T,B
U2B
T�B
T�B
T�B
T�B
T�B
UB
UB
U2B
UgB
VB
W$B
W?B
W$B
W$B
W$B
W$B
XEB
X_B
XEB
X+B
Y1B
Y1B
YB
Y1B
Y1B
Y1B
YKB
ZQB
[=B
[	B
[#B
[=B
[#B
[WB
\]B
]/B
]/B
]/B
]/B
]IB
]IB
]IB
^5B
^5B
^OB
^OB
^jB
^OB
_pB
_VB
_VB
_VB
_;B
_!B
_VB
_VB
_pB
`vB
abB
abB
abB
aHB
abB
abB
abB
aHB
a|B
a�B
b�B
cnB
cnB
cnB
cnB
dtB
dtB
e`B
e`B
ezB
ezB
ezB
e�B
f�B
f�B
f�B
ffB
f�B
gmB
gmB
g�B
g�B
g�B
f�B
ffB
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
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
mwB
m�B
m�B
m�B
n�B
m�B
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
p�B
p�B
p�B
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
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y	B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
y�B
y�B
y�B
y�B
z�B
z�B
z�B
{B
z�B
z�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610040036162016100400361620161004003616201806221302522018062213025220180622130252201804050702222018040507022220180405070222  JA  ARFMdecpA19c                                                                20160930063504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160929213514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160929213515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160929213515  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160929213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160929213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160929213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160929213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160929213516  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160929213517                      G�O�G�O�G�O�                JA  ARUP                                                                        20160929223254                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160930153317  CV  JULD            G�O�G�O�F�u�                JM  ARCAJMQC2.0                                                                 20161003153616  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161003153616  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220222  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040252  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                