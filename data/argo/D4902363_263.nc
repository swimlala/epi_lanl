CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-25T00:35:20Z creation;2018-07-25T00:35:24Z conversion to V3.1;2019-12-19T07:36:43Z update;     
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
resolution        =���   axis      Z        l  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  st   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ې   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20180725003520  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_263                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�tvKHp�1   @�tw`� @9������dV�\)1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�3D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D�|�Dܼ�D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�3D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D�|�Dܼ�D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�5?A��A��#A��
A���A���A���A���A���A���A���A���A���A�ĜAϩ�A��A�bNAǣ�A��
A�/A�Q�A���A��TA�;dA��9A���A���A��A�-A��A�VA���A��A���A�A���A�VA���A��A�{A��+A��FA�z�A��hA���A�S�A��A��jA���A��HA�=qA�bNA��mA���A�M�A�9XA�  A��HA��#A�dZA��A�ZA�A�;dA���A���A�bNA���A���A�+A��A��A�r�A��A�+A�ffA���A�%A��A�E�A�"�A�VA���A��mA��`A��A��#A�&�A~��A}�A|�A{��Ay�AxȴAw%Ar��Aq�Ao�PAmK�Ak��Ak
=Ai��AihsAh�uAf��Af �Aex�Ae;dAdbNAb�yAa�A_��A^9XA\�!AZ1'AWAU��AT�`AT�ATbNAP��AN��ANZANZANI�AMƨAMO�ALA�AI�mAHn�AG�mAG��AGK�AFĜAF5?AE
=AD=qAD�ADbAC��ABffAA;dA?��A?�A=��A=l�A<��A;�
A;�A:��A:9XA9x�A8�uA7�-A7/A6��A6 �A5&�A4��A4$�A29XA1�A133A0�A0��A0��A/��A/?}A.bNA-��A-�TA-A+��A*z�A)�A)��A)p�A(��A(E�A'��A&�yA%�hA$�HA#�hA#33A#+A#�A"�HA"I�A!C�A ZA��A�A��A/A  Al�A�A1'AXAI�AAp�A/AȴAZA �A��AA�jA{AhsA�yAȴA��AQ�A��A�9A��A  A
A�A�`AE�A�wA^5A��A�PA�AO�AoA�A�+A�AƨA�`A�7A bN@�ȴ@�x�@��@���@�Q�@�+@��\@���@�$�@�t�@�\@���@�V@��@��@�\)@噚@�@�;d@���@�Z@�n�@�/@ܬ@��y@��/@ղ-@���@�|�@ҸR@���@Гu@�b@϶F@�"�@�-@�/@�Q�@�E�@�-@�-@�5?@�-@�-@�=q@�E�@�E�@�E�@�V@�n�@�n�@�E�@�V@��;@��T@���@å�@�K�@��@�E�@��9@���@���@���@���@��H@�n�@�J@���@��j@�o@�5?@�7L@��j@���@�I�@�l�@��\@��@���@���@���@���@�j@�ff@��@�bN@��@���@���@���@�X@�z�@���@�dZ@��@�=q@��#@�@���@�hs@��@�l�@�33@���@�E�@��@���@�7L@�Z@��@��^@��@��@���@���@�I�@�t�@�+@��@�~�@�-@��h@�1'@�dZ@�o@���@�@�7L@���@�r�@�1@�|�@���@�@���@�x�@��@��@�1'@��m@���@�dZ@�K�@��@���@��;@���@�~�@�v�@�n�@��@���@��@�hs@�hs@��@�A�@�1'@�b@���@��P@��@��@�|�@�|�@�dZ@�;d@�"�@��@���@�V@��@���@��@�p�@�O�@��9@�Q�@�I�@�A�@�@|�@
=@~ȴ@~�+@~5?@~@}�T@}p�@}�@|��@|�@|�@|I�@|(�@{�F@{dZ@{o@z�@z~�@z-@z�@zJ@y�@y�7@y7L@x��@xr�@xb@w��@w�P@w+@w
=@v�y@v�+@vE�@v5?@v{@u�h@uV@tj@s�m@s��@s"�@r��@r~�@rM�@q�@q��@qx�@qhs@qX@p��@p��@pQ�@p  @o�w@oK�@n�@nff@m�T@m/@l��@lI�@k��@k�
@k�F@k�@kS�@k"�@j�H@j�\@j~�@jn�@j-@i��@i�^@ihs@iG�@h��@hĜ@h��@hr�@h  @g�@g�w@g�@fff@f{@f@e�@e��@e@e��@e�-@e�@eO�@d�@dz�@dZ@d�@c�m@c��@cC�@c@b��@b�@a�^@a��@a��@ahs@aG�@a7L@a�@`Ĝ@`�u@`b@_�;@_�P@_\)@_;d@_
=@^ȴ@^v�@^$�@]�T@]�@]/@\��@\I�@[�F@Z�@Z�\@Zn�@Zn�@Z^5@Z^5@Z^5@ZM�@Z�@Y�#@Y�7@Y7L@X�`@XbN@Xb@W�@W��@W|�@W|�@Wl�@WK�@W�@Vȴ@V��@U�T@UO�@T�j@TI�@S��@S��@S"�@R��@R^5@RM�@RM�@R-@Q�#@QX@P�9@P�@PbN@P1'@P  @O�@O�@Ol�@N�y@NV@N5?@M�@M��@MV@Lj@K��@K��@KdZ@J��@J~�@J=q@I��@Ihs@H�`@HQ�@G��@G��@G\)@G
=@F�@F�@F�R@F��@F�+@Fv�@FV@F$�@F@E��@E?}@E�@D�/@DZ@D9X@D�@C�m@Cƨ@CdZ@B��@BM�@A�@A��@A��@A��@AX@A%@@�9@@bN@@  @>�y@>��@>�+@>�+@>�+@>V@=�T@=�h@=O�@<��@<��@<z�@<Z@<1@;��@;33@:�H@:��@:~�@:=q@9�^@9hs@9X@9&�@9%@9%@8�`@8Ĝ@8�@8bN@8b@7��@7��@7\)@6��@5�@5��@5/@4�D@41@3��@3ƨ@3�F@3�F@3��@3�@3dZ@333@3"�@2�H@2��@2��@2^5@2M�@2=q@2-@2J@1�#@1x�@1&�@0��@0 �@/��@/�P@/�@.ȴ@.��@.��@.�+@.�+@.V@.@-?}@,��@,�@,��@,�@,�D@,Z@,�@+ƨ@+�F@+��@+dZ@+S�@+C�@*�@*n�@*J@*J@)��@)��@)7L@(�`@(Ĝ@(Q�@'��@'�P@'l�@';d@&�R@&V@&5?@&{@&{@%��@%�@%�@%�@%O�@$�/@$�j@$�@$Z@$�@#�
@#�@#C�@#C�@#33@#33@#o@"��@"^5@"=q@"=q@"=q@"=q@!�#@!��@!��@!�7@!x�@!G�@!�@ ��@ �@ A�@  �@�@�;@��@;d@�y@ȴ@��@v�@E�@$�@@�T@�-@�@`B@O�@?}@�@V@�@�@�/@��@�j@�j@�D@j@(�@�m@t�@��@^5@=q@-@��@�#@�^@��@��@x�@%@�9@��@�u@bN@b@b@�@�;@�w@\)@K�@
=@v�@V@5?@$�@��@��@�h@`B@�@��@�/@�@��@j@Z@I�@(�@��@�
@��@C�@33@�H@^5@J@��@�@��@��@x�@hs@X@G�@7L@�@Ĝ@�9@�u@�@Q�@b@�w@�P@l�@�@
=@�y@ff@{@@�T@��@@��@O�@/@�@�@�@�/@��@j@Z@9X@�@1@1@��@�m@�F@�@dZ@C�@33@@
�@
�H@
��@
n�@
�@	�@	��@	X@	�@�`@��@�`@�`@��@Ĝ@�9@r�@b@�w@��@�P@\)@�@�@�@�@�@��@��@��@��@�+@�+@V@@�@�T@��@�@/@�@�j@z�@1@�m@�F@��@t�@S�@C�@33@33@�@��@n�@�#@�7@hs@X@G�@G�@%@ Ĝ@ �u@ bN@ A�@ b@   ?��w?���?���?�|�?�;d?��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�5?A��A��#A��
A���A���A���A���A���A���A���A���A���A�ĜAϩ�A��A�bNAǣ�A��
A�/A�Q�A���A��TA�;dA��9A���A���A��A�-A��A�VA���A��A���A�A���A�VA���A��A�{A��+A��FA�z�A��hA���A�S�A��A��jA���A��HA�=qA�bNA��mA���A�M�A�9XA�  A��HA��#A�dZA��A�ZA�A�;dA���A���A�bNA���A���A�+A��A��A�r�A��A�+A�ffA���A�%A��A�E�A�"�A�VA���A��mA��`A��A��#A�&�A~��A}�A|�A{��Ay�AxȴAw%Ar��Aq�Ao�PAmK�Ak��Ak
=Ai��AihsAh�uAf��Af �Aex�Ae;dAdbNAb�yAa�A_��A^9XA\�!AZ1'AWAU��AT�`AT�ATbNAP��AN��ANZANZANI�AMƨAMO�ALA�AI�mAHn�AG�mAG��AGK�AFĜAF5?AE
=AD=qAD�ADbAC��ABffAA;dA?��A?�A=��A=l�A<��A;�
A;�A:��A:9XA9x�A8�uA7�-A7/A6��A6 �A5&�A4��A4$�A29XA1�A133A0�A0��A0��A/��A/?}A.bNA-��A-�TA-A+��A*z�A)�A)��A)p�A(��A(E�A'��A&�yA%�hA$�HA#�hA#33A#+A#�A"�HA"I�A!C�A ZA��A�A��A/A  Al�A�A1'AXAI�AAp�A/AȴAZA �A��AA�jA{AhsA�yAȴA��AQ�A��A�9A��A  A
A�A�`AE�A�wA^5A��A�PA�AO�AoA�A�+A�AƨA�`A�7A bN@�ȴ@�x�@��@���@�Q�@�+@��\@���@�$�@�t�@�\@���@�V@��@��@�\)@噚@�@�;d@���@�Z@�n�@�/@ܬ@��y@��/@ղ-@���@�|�@ҸR@���@Гu@�b@϶F@�"�@�-@�/@�Q�@�E�@�-@�-@�5?@�-@�-@�=q@�E�@�E�@�E�@�V@�n�@�n�@�E�@�V@��;@��T@���@å�@�K�@��@�E�@��9@���@���@���@���@��H@�n�@�J@���@��j@�o@�5?@�7L@��j@���@�I�@�l�@��\@��@���@���@���@���@�j@�ff@��@�bN@��@���@���@���@�X@�z�@���@�dZ@��@�=q@��#@�@���@�hs@��@�l�@�33@���@�E�@��@���@�7L@�Z@��@��^@��@��@���@���@�I�@�t�@�+@��@�~�@�-@��h@�1'@�dZ@�o@���@�@�7L@���@�r�@�1@�|�@���@�@���@�x�@��@��@�1'@��m@���@�dZ@�K�@��@���@��;@���@�~�@�v�@�n�@��@���@��@�hs@�hs@��@�A�@�1'@�b@���@��P@��@��@�|�@�|�@�dZ@�;d@�"�@��@���@�V@��@���@��@�p�@�O�@��9@�Q�@�I�@�A�@�@|�@
=@~ȴ@~�+@~5?@~@}�T@}p�@}�@|��@|�@|�@|I�@|(�@{�F@{dZ@{o@z�@z~�@z-@z�@zJ@y�@y�7@y7L@x��@xr�@xb@w��@w�P@w+@w
=@v�y@v�+@vE�@v5?@v{@u�h@uV@tj@s�m@s��@s"�@r��@r~�@rM�@q�@q��@qx�@qhs@qX@p��@p��@pQ�@p  @o�w@oK�@n�@nff@m�T@m/@l��@lI�@k��@k�
@k�F@k�@kS�@k"�@j�H@j�\@j~�@jn�@j-@i��@i�^@ihs@iG�@h��@hĜ@h��@hr�@h  @g�@g�w@g�@fff@f{@f@e�@e��@e@e��@e�-@e�@eO�@d�@dz�@dZ@d�@c�m@c��@cC�@c@b��@b�@a�^@a��@a��@ahs@aG�@a7L@a�@`Ĝ@`�u@`b@_�;@_�P@_\)@_;d@_
=@^ȴ@^v�@^$�@]�T@]�@]/@\��@\I�@[�F@Z�@Z�\@Zn�@Zn�@Z^5@Z^5@Z^5@ZM�@Z�@Y�#@Y�7@Y7L@X�`@XbN@Xb@W�@W��@W|�@W|�@Wl�@WK�@W�@Vȴ@V��@U�T@UO�@T�j@TI�@S��@S��@S"�@R��@R^5@RM�@RM�@R-@Q�#@QX@P�9@P�@PbN@P1'@P  @O�@O�@Ol�@N�y@NV@N5?@M�@M��@MV@Lj@K��@K��@KdZ@J��@J~�@J=q@I��@Ihs@H�`@HQ�@G��@G��@G\)@G
=@F�@F�@F�R@F��@F�+@Fv�@FV@F$�@F@E��@E?}@E�@D�/@DZ@D9X@D�@C�m@Cƨ@CdZ@B��@BM�@A�@A��@A��@A��@AX@A%@@�9@@bN@@  @>�y@>��@>�+@>�+@>�+@>V@=�T@=�h@=O�@<��@<��@<z�@<Z@<1@;��@;33@:�H@:��@:~�@:=q@9�^@9hs@9X@9&�@9%@9%@8�`@8Ĝ@8�@8bN@8b@7��@7��@7\)@6��@5�@5��@5/@4�D@41@3��@3ƨ@3�F@3�F@3��@3�@3dZ@333@3"�@2�H@2��@2��@2^5@2M�@2=q@2-@2J@1�#@1x�@1&�@0��@0 �@/��@/�P@/�@.ȴ@.��@.��@.�+@.�+@.V@.@-?}@,��@,�@,��@,�@,�D@,Z@,�@+ƨ@+�F@+��@+dZ@+S�@+C�@*�@*n�@*J@*J@)��@)��@)7L@(�`@(Ĝ@(Q�@'��@'�P@'l�@';d@&�R@&V@&5?@&{@&{@%��@%�@%�@%�@%O�@$�/@$�j@$�@$Z@$�@#�
@#�@#C�@#C�@#33@#33@#o@"��@"^5@"=q@"=q@"=q@"=q@!�#@!��@!��@!�7@!x�@!G�@!�@ ��@ �@ A�@  �@�@�;@��@;d@�y@ȴ@��@v�@E�@$�@@�T@�-@�@`B@O�@?}@�@V@�@�@�/@��@�j@�j@�D@j@(�@�m@t�@��@^5@=q@-@��@�#@�^@��@��@x�@%@�9@��@�u@bN@b@b@�@�;@�w@\)@K�@
=@v�@V@5?@$�@��@��@�h@`B@�@��@�/@�@��@j@Z@I�@(�@��@�
@��@C�@33@�H@^5@J@��@�@��@��@x�@hs@X@G�@7L@�@Ĝ@�9@�u@�@Q�@b@�w@�P@l�@�@
=@�y@ff@{@@�T@��@@��@O�@/@�@�@�@�/@��@j@Z@9X@�@1@1@��@�m@�F@�@dZ@C�@33@@
�@
�H@
��@
n�@
�@	�@	��@	X@	�@�`@��@�`@�`@��@Ĝ@�9@r�@b@�w@��@�P@\)@�@�@�@�@�@��@��@��@��@�+@�+@V@@�@�T@��@�@/@�@�j@z�@1@�m@�F@��@t�@S�@C�@33@33@�@��@n�@�#@�7@hs@X@G�@G�@%@ Ĝ@ �u@ bN@ A�@ b@   ?��w?���?���?�|�?�;d?��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�^B�}B��B��B��BBBÖBÖBÖBÖBÖBB�qB��BB�B_;B�B�hB�%B�VB�7Bm�Bu�B�B��B�bB�uB�VB}�BgmBgmBXBhsBdZBXBK�B33BhB	7B�BB��B�B�B�B�B�wBB�B��B�PB��B��B��B�Br�Bs�Bl�B`BBYBD�B&�B�B	7B	7B
�B
�mB
ɺB
ɺB
ȴB
�qB
�B
�+B
�VB
�B
ZB
VB
�B
�B
�B
�B
�B
�B
z�B
gmB
[#B
ZB
ZB
R�B
:^B
5?B
�B	��B
B	��B	�yB	�B	�B	�mB	�fB	�5B	��B	��B	��B	��B	ƨB	�?B	�'B	��B	��B	�DB	p�B	e`B	_;B	hsB	m�B	bNB	>wB	5?B	M�B	VB	Q�B	F�B	A�B	.B	�B	�B	"�B	&�B	!�B	�B	�B	hB	VB	�B	�B	PB��B��B�B�B�TB�B�ZB�/B�/B�BB�B��B��BǮB��BɺBĜB�^B��B�?B��B�!B�RB�LB�LB�3B��B��B��B��B��B��B�DB�\B��B��B��B�hB�{B�\B�B~�B~�B~�B�+B�VB�DB�B{�Br�Bq�Bq�Bk�Bn�Bn�BffBl�BjBdZBcTB\)BgmBgmBgmBcTB_;B]/BS�B@�BL�BN�BP�BR�BXBVBN�BD�B:^B7LB%�B)�B,B;dB9XB1'B49BB�BB�B?}B<jB<jB6FB2-B1'B$�B�B�B�B#�B$�B!�BPB�B�BoB%B	7B{B�BoBB+B	7B1B%BhBBbB1B	7B\BB��B��B1BuBbB
=B�B�B�B�BbBoBuBbB(�B-B.B.B.B/B.B.B.B-B-B)�B$�B�B�B�B�B0!B33B1'B,B%�B"�B6FB/B0!B.B<jB>wB;dB5?B2-B;dB>wBE�BH�BE�B?}BA�BC�B@�B;dBJ�BN�BP�BG�BK�BXBXB\)B]/BhsBjBgmBk�Bo�Bp�Bq�Bv�Bx�Bv�Bt�Bm�Bw�B|�B{�B~�B� B� B~�B{�B}�B�B�uB�uB��B��B��B��B��B��B��B��B��B��B��B�'B�-B�3B�RB�}BÖBĜBŢBǮB��B��B��B��B�B�5B�HB�ZB�fB�mB�TB�;B�NB��B	B	1B	1B	+B	1B	DB	VB	\B	JB	PB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	'�B	)�B	,B	+B	)�B	0!B	6FB	7LB	6FB	:^B	@�B	B�B	C�B	C�B	E�B	F�B	G�B	J�B	L�B	M�B	M�B	N�B	Q�B	Q�B	T�B	W
B	YB	YB	]/B	_;B	`BB	_;B	_;B	aHB	cTB	e`B	gmB	iyB	k�B	l�B	n�B	o�B	o�B	q�B	s�B	r�B	q�B	t�B	v�B	z�B	}�B	�B	�B	�B	�+B	�1B	�=B	�JB	�VB	�VB	�VB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�LB	�RB	�RB	�RB	�dB	�^B	�XB	�dB	��B	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�/B	�5B	�BB	�TB	�fB	�fB	�fB	�mB	�mB	�fB	�fB	�fB	�fB	�fB	�mB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B
  B
B
B
B
B
%B
+B
%B
+B
1B
1B

=B
DB
JB
JB
PB
\B
\B
\B
bB
bB
bB
bB
bB
\B
bB
oB
hB
hB
uB
uB
uB
uB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
"�B
"�B
"�B
#�B
%�B
%�B
%�B
&�B
&�B
%�B
%�B
&�B
%�B
&�B
&�B
%�B
$�B
$�B
(�B
&�B
'�B
+B
-B
-B
.B
.B
.B
.B
.B
.B
/B
.B
/B
/B
/B
0!B
0!B
0!B
/B
/B
.B
.B
.B
.B
1'B
1'B
1'B
33B
49B
5?B
5?B
49B
33B
2-B
1'B
5?B
7LB
7LB
7LB
7LB
7LB
7LB
7LB
9XB
9XB
8RB
9XB
9XB
7LB
7LB
8RB
;dB
:^B
9XB
9XB
:^B
;dB
:^B
:^B
<jB
=qB
=qB
<jB
=qB
?}B
@�B
@�B
?}B
?}B
A�B
A�B
@�B
?}B
A�B
A�B
@�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
C�B
B�B
C�B
E�B
E�B
E�B
E�B
D�B
E�B
F�B
F�B
F�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
F�B
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
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
L�B
M�B
L�B
L�B
K�B
K�B
K�B
J�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
P�B
O�B
P�B
R�B
R�B
Q�B
R�B
S�B
S�B
S�B
S�B
R�B
T�B
S�B
R�B
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
XB
YB
YB
YB
ZB
YB
YB
[#B
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
\)B
^5B
^5B
^5B
^5B
]/B
^5B
_;B
_;B
_;B
`BB
_;B
^5B
_;B
aHB
aHB
bNB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
bNB
bNB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
dZB
dZB
e`B
e`B
ffB
ffB
gmB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
iyB
iyB
jB
jB
iyB
iyB
iyB
hsB
hsB
iyB
k�B
k�B
jB
k�B
l�B
l�B
l�B
l�B
l�B
k�B
m�B
n�B
n�B
m�B
m�B
m�B
n�B
n�B
m�B
n�B
m�B
m�B
n�B
n�B
m�B
o�B
o�B
p�B
o�B
p�B
q�B
q�B
q�B
p�B
p�B
o�B
n�B
p�B
r�B
r�B
r�B
r�B
r�B
q�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��BBBÖBÖBÖBðB��B�GB�cB�}B
�B��BkB�(B�aB��B��B��Bt�By�B��B�'B�oB��B�vB�4Bk6BjKB[WBiDBeBX�BL�B5B{BB+B+B�BB�B�B�WB�)B��B�3B��B��B��B�NB�@B��B��Bt�Bt�Bm�Ba�BZQBF�B*BB�B
�B
�+B
�DB
�B
�xB
��B
��B
�CB
�B
�.B
��B
_B
YeB
�-B
�MB
�MB
�3B
�3B
�AB
{B
iDB
]B
[qB
[=B
T,B
<�B
6�B
"�B	�jB
�B	�DB	�B	�CB	�B	��B	�8B	ߊB	ѷB	��B	ҽB	уB	��B	�LB	��B	�B	��B	��B	s�B	hXB	a|B	i_B	nB	c�B	BuB	72B	N<B	VB	R:B	GzB	BuB	/�B	xB	IB	#�B	'RB	"hB	pB	�B	�B	\B	�B	�B	<B��B�DB�CB��B��B�6B�`B�OB�B�B�B� B�B��B�jB�rBŢB��B� B�zB�>B��B��B��B��B��B��B��B��B��B�fB��B�B��B�1B�)B�)B��B�B�HB��B��B�B�iB��B�pB��B��B|�BtBr�Br�BmBo�Bo�Bh
Bm]BkQBezBd�B]�Bg�Bg�Bg�Bc�B_�B]�BU2BB�BNBO�BQ�BS�BXEBVmBO�BE�B<B8�B(>B,=B-�B<6B:^B2�B5%BB�BB�B?�B<�B<�B6�B2�B1�B&LBkB#B�B$�B%zB"�B�BdBIB�BB
�BBB&B�BKB
rB	lBzB�BSB�B	lB
	B�BgB �B��B	B�B B^B�B
B�BBB&BFB�B(�B-B./B./B.IB/B./B./B./B-)B-)B*0B%FB�B�BB�B0UB3�B1�B,�B'B$&B6`B0B0�B/OB<�B>�B;�B6B3�B<B?.BFBH�BFB@OBBABD3BA�B<�BKxBOvBQ�BI7BL�BX�BX�B\�B]�Bh�Bj�Bh$BlBpBq'Br-BwBx�BwBu%Bn�Bx8B}"B|jBHB�OB�OB}B|�B~�B��B��B��B��B��B��B�+B�-B� B�:B�@B�nB��B��B�vB��B��B��B��B��B�B�%B�KB�DB�&B�@B�aBևB�jB�|B�B�B�B��B�'B�B�`B	3B	KB	KB	zB	�B	xB	pB	vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	!B	#:B	($B	*0B	,"B	+6B	*eB	0UB	6FB	7fB	6zB	:�B	@�B	B�B	C�B	C�B	E�B	F�B	G�B	J�B	L�B	M�B	NB	OB	RB	R B	UB	W$B	Y1B	YKB	]/B	_VB	`\B	_pB	_�B	a|B	c�B	e�B	g�B	i�B	k�B	l�B	n�B	o�B	o�B	q�B	s�B	r�B	q�B	t�B	wB	{B	~(B	�;B	�'B	�3B	�EB	�fB	�XB	�dB	�pB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�)B	�)B	�5B	�OB	�5B	�5B	�;B	�AB	�MB	�nB	�LB	�lB	�lB	��B	�dB	�xB	��B	��B	��B	ÖB	ĜB	ĶB	żB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�"B	�B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�9B	�$B	�+B	�1B	�=B	�=B	�=B	�WB	�CB	�IB	�~B	�OB	�VB	�~B	�jB	��B	�nB	�fB	�fB	�fB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	�B	�B	�B	�B	�B	�B	�B	�<B
 B
 B
 4B	�HB
 4B
'B
3B
3B
MB
?B
EB
YB
_B
�B
fB

rB
DB
dB
dB
jB
\B
vB
vB
bB
}B
}B
}B
}B
�B
}B
�B
�B
�B
�B
uB
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
 �B
 �B
#�B
"�B
"�B
#B
#�B
%�B
%�B
%�B
&�B
'B
%�B
%�B
'B
&B
'B
'B
%�B
%B
%FB
)B
'8B
($B
+6B
-B
-)B
./B
.B
.IB
.B
./B
./B
/B
./B
/5B
/5B
/5B
0;B
0!B
0;B
/5B
/5B
.IB
.IB
.IB
.cB
1AB
1AB
1[B
3MB
4TB
5%B
5?B
49B
3MB
2aB
1vB
5ZB
7LB
7fB
7fB
7fB
7fB
7fB
7fB
9XB
9rB
8RB
9XB
9rB
7fB
7�B
8lB
;dB
:xB
9�B
9�B
:xB
;dB
:�B
:�B
<�B
=�B
=qB
<�B
=�B
?�B
@�B
@�B
?�B
?�B
A�B
A�B
@�B
?�B
A�B
A�B
@�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
C�B
B�B
C�B
E�B
E�B
E�B
E�B
D�B
E�B
F�B
F�B
F�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
F�B
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
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
L�B
M�B
L�B
L�B
K�B
K�B
K�B
KB
N�B
O�B
O�B
PB
Q B
QB
Q B
Q�B
Q B
PB
Q B
R�B
SB
RB
SB
S�B
TB
TB
T,B
S&B
UB
TB
S&B
VB
VB
VB
VB
W$B
W$B
W
B
W
B
X+B
X+B
XEB
XEB
X+B
YB
YB
Y1B
X+B
Y1B
Y1B
Y1B
Z7B
YKB
YKB
[WB
\)B
\CB
\CB
]/B
]IB
]/B
]/B
]/B
]IB
]IB
\CB
^5B
^OB
^5B
^OB
]IB
^OB
_VB
_VB
_VB
`\B
_VB
^jB
_VB
aHB
aHB
bNB
a|B
abB
abB
b�B
c:B
cTB
cTB
bhB
bhB
cnB
dZB
dtB
dtB
e`B
e`B
eFB
e`B
dtB
dtB
ezB
ezB
ffB
f�B
g�B
f�B
f�B
ffB
ffB
f�B
f�B
g�B
g�B
i�B
iyB
jB
jB
iyB
i_B
i�B
h�B
h�B
iyB
k�B
k�B
j�B
k�B
l�B
l�B
l�B
l�B
l�B
k�B
m�B
n�B
n�B
m�B
m�B
m�B
n�B
n�B
m�B
n�B
m�B
m�B
n�B
n�B
m�B
o�B
o�B
p�B
o�B
p�B
q�B
q�B
q�B
p�B
p�B
o�B
n�B
p�B
r�B
r�B
r�B
r�B
r�B
q�B
r�B
s�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<>�<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807290039282018072900392820180729003928201807290200202018072902002020180729020020201807300028052018073000280520180730002805  JA  ARFMdecpA19c                                                                20180725093509  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180725003520  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180725003522  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180725003523  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180725003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180725003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180725003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180725003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180725003524  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180725003524                      G�O�G�O�G�O�                JA  ARUP                                                                        20180725005557                      G�O�G�O�G�O�                JA  ARUP                                                                        20180727041505                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180725153405  CV  JULD            G�O�G�O�Fã�                JM  ARCAJMQC2.0                                                                 20180728153928  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180728153928  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180728170020  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180729152805  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                