CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-09T00:35:18Z creation;2018-08-09T00:35:23Z conversion to V3.1;2019-12-19T07:35:34Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180809003518  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_268                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�x4* 1   @�x4��J @9�ȴ9X�d^�J�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B��B(  B0  B8  B@  BH  BP  BW��B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�<�Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B��B(  B0  B8  B@  BH  BP  BW��B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�<�Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A�O�A�E�A�?}A�5?A��A�JA��A˴9A�l�A�ffA�dZA�^5A��Aɉ7A�|�AǋDA�A���A�|�A��A�JA�JA��A���A�oA��FA� �A���A��#A��!A�-A�x�A�+A�n�A���A��/A�VA�&�A���A�A�?}A�ZA��-A�I�A�~�A��A��9A�n�A�?}A��A��A� �A��A�Q�A��A��wA�|�A�/A���A�hsA��+A��`A�v�A�r�A�O�A�E�A�&�A�bA���A�/A�bA��A�
=A���A��+A�Q�A���A�;dA��9A�VA�9XA�A~��A~��A~I�A}ƨA|�uAx��Aw��At�!Ar�DAq��Ap��ApbAn�HAn9XAkt�Ai��Ah(�Ad�DAcC�Ac�Ab�/Ab�!A`��A^�A]��AZ�HAZjAY�AY
=AX �AWt�AV�`AVffAU��AU&�AT=qAR�uAP��AO"�AN�`ANȴAN�jAN�AM%AKoAJ�AIS�AHA�AGG�AF��AF�`AF�/AF�AF��AE��AEO�AC�mAB�A@�!A@E�A?�TA?��A>�yA=hsA<1'A;`BA:��A9�hA8n�A8E�A7��A6n�A6^5A5�A5+A3|�A3C�A2Q�A0��A/��A/�FA.��A-XA, �A+
=A*��A*�HA*�uA*ZA*E�A*�A)�
A)��A)��A)33A'�A'��A'dZA&9XA%7LA#�FA"��A"�A �yA��A
=A��A��AAr�AXA�A7LA�`A�+A(�A%A^5A�;A\)A��AA�AA�mA�A�RA^5A/A��A��AG�A��Az�A�#A7LA
n�A	��A�yAM�A��AG�A��A(�A�-A��A�FA�!AƨA@�@�?}@��@��@��+@���@��@�+@��H@��@��;@��@�?}@��D@�\)@�J@�V@�A�@�S�@�~�@���@�x�@���@�t�@�{@�K�@�J@��`@�dZ@�?}@��@�z�@���@ڟ�@׍P@��@��y@Ώ\@�n�@�-@��@��T@͑h@̴9@��@�33@ə�@��@�V@�hs@�/@�V@�(�@å�@�S�@�^5@��@���@��@�@�-@���@�%@��@�\)@���@���@��@�K�@��H@�$�@�&�@��u@���@��@�n�@�@���@���@��@��@� �@�\)@�o@�
=@�ȴ@���@��\@�v�@�v�@�M�@���@�/@��m@��@�E�@�=q@��#@�  @���@���@�~�@�`B@��9@�A�@���@��@��@���@�n�@���@�x�@��@��@��@�M�@���@���@�A�@���@�|�@��+@�V@�5?@�{@��^@�G�@���@�9X@���@��
@��@���@�K�@��@���@���@�G�@�/@�V@���@�z�@�9X@�b@��;@��w@�33@�@��+@�E�@�{@��@�@�I�@�C�@��@��@���@���@��+@�n�@�^5@�E�@�-@���@���@�r�@��@��P@�\)@�
=@��\@�{@��7@�V@��9@�bN@��F@��@��+@�V@��@���@��7@��@��@���@��u@�Z@��;@��w@��w@��@���@�|�@�S�@���@�ȴ@���@��\@�n�@�5?@�{@���@��#@���@�x�@�&�@�%@���@���@��@��`@��/@��/@��@�Z@�(�@+@}��@}?}@}/@}V@|��@|�D@{dZ@{"�@{@z��@z=q@z-@y��@x��@x�@xr�@xQ�@x �@w�P@vv�@u�-@t�/@t�D@tj@tZ@tZ@t9X@s��@s��@sC�@r�@r~�@qhs@p��@p��@p��@p�u@p�@pA�@o|�@n��@nȴ@n�R@n@m�@l��@l�/@lz�@k�
@k"�@j�!@jn�@jJ@i��@ihs@iG�@i&�@i&�@h�`@h�9@hbN@h �@g��@g;d@f��@f�@fff@e�@e/@d��@d�j@d1@c�F@c33@b�@bn�@b=q@b=q@b-@bJ@a�#@a��@a�@`��@`��@`�9@`�u@`r�@`Q�@_�@_+@^ff@^$�@^$�@^@]�T@]p�@]/@\��@\�/@\�j@\z�@\�@[ƨ@[��@[t�@[dZ@[33@Z�@Z�H@Z��@Z��@Z=q@Y��@X��@X1'@W�P@W+@W
=@V�@Vv�@V$�@U�@U/@UV@T�/@T�D@T(�@S�
@S��@SS�@R��@RM�@Q�^@QX@Q%@P�`@P�9@P��@P�u@P�@PbN@Pb@P  @P  @O�@O\)@N��@N�R@Nff@N{@M��@M�@M/@L��@L�/@L��@Lj@L(�@Kƨ@Kt�@KS�@Ko@J~�@J�@JJ@I�#@Ix�@IX@H�`@H�u@HbN@H  @G\)@F�y@F$�@E�-@E`B@E/@E�@EV@D�j@D�D@DZ@C��@C��@CS�@C"�@B�@B~�@B-@B�@A�#@A�^@A��@AX@A%@@Ĝ@@bN@@ �@?�w@?��@?;d@>��@>ȴ@>ff@>5?@=�@<�@<j@<Z@<(�@;��@;�@;o@:�H@:n�@9�#@9��@9�7@97L@8�`@8�u@8A�@7�@7�@7|�@7+@6�@6��@6E�@6@5��@5p�@5/@4�@4�j@4I�@3S�@2�@2�!@2��@2~�@2=q@1�^@1&�@0�`@0�u@0A�@/�@/��@/K�@.�R@.@-O�@,�@,�/@,��@,��@,�@+�@+33@+o@*�H@*��@*=q@)�#@)X@(��@(�`@(�`@(��@(��@(r�@'��@'�P@'\)@';d@'�@&�y@&�@&��@&E�@&E�@&5?@&{@%��@%��@%p�@%p�@%O�@%�@%�@%V@$�/@$�@$�@$z�@$j@$(�@#�F@#t�@#S�@#C�@"��@"�!@"^5@"=q@"-@"J@!�@!��@!x�@!7L@!%@ �`@ �u@ b@�w@l�@�@�R@�+@�+@�+@v�@ff@$�@@�@z�@I�@�@�
@�F@�@S�@C�@o@�@�H@��@�!@~�@^5@^5@=q@�@��@�@��@hs@X@7L@&�@�@��@�9@r�@bN@1'@�P@l�@\)@l�@\)@;d@�y@ȴ@�R@��@��@��@�+@v�@ff@E�@{@@�@�-@`B@`B@`B@O�@?}@�@V@V@V@��@�@�/@�@j@9X@(�@�@�@��@33@�@��@�!@�\@-@J@�^@G�@�@��@��@Ĝ@�@�@bN@1'@�;@�@|�@\)@;d@
=@�R@�+@ff@ff@V@E�@@�-@�@O�@�@��@�j@�D@z�@I�@(�@�@�@�
@�F@��@S�@o@
��@
~�@
^5@
^5@
-@
J@	�@	��@	��@	x�@	G�@	%@Ĝ@r�@A�@  @��@�@|�@\)@;d@;d@;d@�@
=@�@�R@��@V@5?@$�@{@��@��@`B@��@�j@��@z�@Z@9X@(�@(�@(�@1@�
@dZ@C�@C�@33@"�@o@�H@�H@�@�@��@��@�!@�\@~�@n�@-@J@J@��@�@��@��@x�@X@7L@�@ Ĝ@ �@ Q�@ Q�@ A�@ A�@ 1'@   ?��;?���?�\)?��?��?���?��?��?���?�5??���?��h?�O�?�V?��?��?��?�1?�ƨ?�ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�Q�A�O�A�E�A�?}A�5?A��A�JA��A˴9A�l�A�ffA�dZA�^5A��Aɉ7A�|�AǋDA�A���A�|�A��A�JA�JA��A���A�oA��FA� �A���A��#A��!A�-A�x�A�+A�n�A���A��/A�VA�&�A���A�A�?}A�ZA��-A�I�A�~�A��A��9A�n�A�?}A��A��A� �A��A�Q�A��A��wA�|�A�/A���A�hsA��+A��`A�v�A�r�A�O�A�E�A�&�A�bA���A�/A�bA��A�
=A���A��+A�Q�A���A�;dA��9A�VA�9XA�A~��A~��A~I�A}ƨA|�uAx��Aw��At�!Ar�DAq��Ap��ApbAn�HAn9XAkt�Ai��Ah(�Ad�DAcC�Ac�Ab�/Ab�!A`��A^�A]��AZ�HAZjAY�AY
=AX �AWt�AV�`AVffAU��AU&�AT=qAR�uAP��AO"�AN�`ANȴAN�jAN�AM%AKoAJ�AIS�AHA�AGG�AF��AF�`AF�/AF�AF��AE��AEO�AC�mAB�A@�!A@E�A?�TA?��A>�yA=hsA<1'A;`BA:��A9�hA8n�A8E�A7��A6n�A6^5A5�A5+A3|�A3C�A2Q�A0��A/��A/�FA.��A-XA, �A+
=A*��A*�HA*�uA*ZA*E�A*�A)�
A)��A)��A)33A'�A'��A'dZA&9XA%7LA#�FA"��A"�A �yA��A
=A��A��AAr�AXA�A7LA�`A�+A(�A%A^5A�;A\)A��AA�AA�mA�A�RA^5A/A��A��AG�A��Az�A�#A7LA
n�A	��A�yAM�A��AG�A��A(�A�-A��A�FA�!AƨA@�@�?}@��@��@��+@���@��@�+@��H@��@��;@��@�?}@��D@�\)@�J@�V@�A�@�S�@�~�@���@�x�@���@�t�@�{@�K�@�J@��`@�dZ@�?}@��@�z�@���@ڟ�@׍P@��@��y@Ώ\@�n�@�-@��@��T@͑h@̴9@��@�33@ə�@��@�V@�hs@�/@�V@�(�@å�@�S�@�^5@��@���@��@�@�-@���@�%@��@�\)@���@���@��@�K�@��H@�$�@�&�@��u@���@��@�n�@�@���@���@��@��@� �@�\)@�o@�
=@�ȴ@���@��\@�v�@�v�@�M�@���@�/@��m@��@�E�@�=q@��#@�  @���@���@�~�@�`B@��9@�A�@���@��@��@���@�n�@���@�x�@��@��@��@�M�@���@���@�A�@���@�|�@��+@�V@�5?@�{@��^@�G�@���@�9X@���@��
@��@���@�K�@��@���@���@�G�@�/@�V@���@�z�@�9X@�b@��;@��w@�33@�@��+@�E�@�{@��@�@�I�@�C�@��@��@���@���@��+@�n�@�^5@�E�@�-@���@���@�r�@��@��P@�\)@�
=@��\@�{@��7@�V@��9@�bN@��F@��@��+@�V@��@���@��7@��@��@���@��u@�Z@��;@��w@��w@��@���@�|�@�S�@���@�ȴ@���@��\@�n�@�5?@�{@���@��#@���@�x�@�&�@�%@���@���@��@��`@��/@��/@��@�Z@�(�@+@}��@}?}@}/@}V@|��@|�D@{dZ@{"�@{@z��@z=q@z-@y��@x��@x�@xr�@xQ�@x �@w�P@vv�@u�-@t�/@t�D@tj@tZ@tZ@t9X@s��@s��@sC�@r�@r~�@qhs@p��@p��@p��@p�u@p�@pA�@o|�@n��@nȴ@n�R@n@m�@l��@l�/@lz�@k�
@k"�@j�!@jn�@jJ@i��@ihs@iG�@i&�@i&�@h�`@h�9@hbN@h �@g��@g;d@f��@f�@fff@e�@e/@d��@d�j@d1@c�F@c33@b�@bn�@b=q@b=q@b-@bJ@a�#@a��@a�@`��@`��@`�9@`�u@`r�@`Q�@_�@_+@^ff@^$�@^$�@^@]�T@]p�@]/@\��@\�/@\�j@\z�@\�@[ƨ@[��@[t�@[dZ@[33@Z�@Z�H@Z��@Z��@Z=q@Y��@X��@X1'@W�P@W+@W
=@V�@Vv�@V$�@U�@U/@UV@T�/@T�D@T(�@S�
@S��@SS�@R��@RM�@Q�^@QX@Q%@P�`@P�9@P��@P�u@P�@PbN@Pb@P  @P  @O�@O\)@N��@N�R@Nff@N{@M��@M�@M/@L��@L�/@L��@Lj@L(�@Kƨ@Kt�@KS�@Ko@J~�@J�@JJ@I�#@Ix�@IX@H�`@H�u@HbN@H  @G\)@F�y@F$�@E�-@E`B@E/@E�@EV@D�j@D�D@DZ@C��@C��@CS�@C"�@B�@B~�@B-@B�@A�#@A�^@A��@AX@A%@@Ĝ@@bN@@ �@?�w@?��@?;d@>��@>ȴ@>ff@>5?@=�@<�@<j@<Z@<(�@;��@;�@;o@:�H@:n�@9�#@9��@9�7@97L@8�`@8�u@8A�@7�@7�@7|�@7+@6�@6��@6E�@6@5��@5p�@5/@4�@4�j@4I�@3S�@2�@2�!@2��@2~�@2=q@1�^@1&�@0�`@0�u@0A�@/�@/��@/K�@.�R@.@-O�@,�@,�/@,��@,��@,�@+�@+33@+o@*�H@*��@*=q@)�#@)X@(��@(�`@(�`@(��@(��@(r�@'��@'�P@'\)@';d@'�@&�y@&�@&��@&E�@&E�@&5?@&{@%��@%��@%p�@%p�@%O�@%�@%�@%V@$�/@$�@$�@$z�@$j@$(�@#�F@#t�@#S�@#C�@"��@"�!@"^5@"=q@"-@"J@!�@!��@!x�@!7L@!%@ �`@ �u@ b@�w@l�@�@�R@�+@�+@�+@v�@ff@$�@@�@z�@I�@�@�
@�F@�@S�@C�@o@�@�H@��@�!@~�@^5@^5@=q@�@��@�@��@hs@X@7L@&�@�@��@�9@r�@bN@1'@�P@l�@\)@l�@\)@;d@�y@ȴ@�R@��@��@��@�+@v�@ff@E�@{@@�@�-@`B@`B@`B@O�@?}@�@V@V@V@��@�@�/@�@j@9X@(�@�@�@��@33@�@��@�!@�\@-@J@�^@G�@�@��@��@Ĝ@�@�@bN@1'@�;@�@|�@\)@;d@
=@�R@�+@ff@ff@V@E�@@�-@�@O�@�@��@�j@�D@z�@I�@(�@�@�@�
@�F@��@S�@o@
��@
~�@
^5@
^5@
-@
J@	�@	��@	��@	x�@	G�@	%@Ĝ@r�@A�@  @��@�@|�@\)@;d@;d@;d@�@
=@�@�R@��@V@5?@$�@{@��@��@`B@��@�j@��@z�@Z@9X@(�@(�@(�@1@�
@dZ@C�@C�@33@"�@o@�H@�H@�@�@��@��@�!@�\@~�@n�@-@J@J@��@�@��@��@x�@X@7L@�@ Ĝ@ �@ Q�@ Q�@ A�@ A�@ 1'@   ?��;?���?�\)?��?��?���?��?��?���?�5??���?��h?�O�?�V?��?��?��?�1?�ƨ?�ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bx�Bw�Bw�Bv�Bu�Bu�Bs�Bq�Bn�Bs�Br�Bn�Be`BO�BXBF�B@�B?}B�BdZB��B��B��B�oB�7BgmB~�B�oB�+Bx�B�B{�B~�Bn�BhsBVBG�B1'BB%B�B
=BB�B�B�B��B�B�B�yB�BB�#B�B��B��B��BɺBĜB�RB�B�bBW
BP�BR�BgmBhsBbNBZBG�B�B
��BDBB
�sB
ɺB
�RB
�FB
��B
�DB
�bB
�B
hsB
l�B
t�B
n�B
bNB
O�B
'�B
33B
{B
VB
�B
PB
DB	��B	�B	�B	�}B	��B	�B	�^B	��B	ǮB	�}B	�B	�{B	��B	|�B	�%B	�%B	�B	�B	�B	�B	~�B	x�B	t�B	iyB	]/B	S�B	Q�B	cTB	dZB	bNB	[#B	E�B	0!B	<jB	5?B	49B	.B	5?B	8RB	7LB	49B	-B	!�B	�B	PB��B	B	JB	
=B	
=B��B�fB�NB�B�NB�)B�B�B�BĜB��B��B��B�B��B�XB��B�B�B��B�PB��B��B�B��B��B��B��B��B��B��B��B��B�7B�hB�bB�B�Bx�B� B~�Bx�Bu�Bz�B}�B{�Bq�BbNBaHBaHBcTBm�BiyBdZBYB^5BcTB\)BXBW
BK�BG�BP�BS�BR�BG�BN�BF�BM�BL�BG�BC�B?}B=qB8RB=qB;dB;dB=qB<jB7LB49B/B"�B&�B#�B$�BuB+B-B&�B�B$�B"�B%�B)�B!�B�B�B&�B%�B �B!�B%�B&�B%�B%�B(�B'�B$�B�B�BbB�B�B�B{B$�B!�B�B\BB��BB)�B,B+B,B(�B%�B"�B �B�B�B�B�B#�B+B+B%�B&�B'�B#�B$�B"�B�B+B)�B.B,B.B2-B:^B?}B?}BA�B>wB;dB;dBA�B?}BH�BH�BN�BN�BN�BJ�BK�BL�BM�BR�BVBT�BT�BVBVBVBR�BM�BM�BK�BK�BVBZBW
BN�BVBdZBbNB_;BaHBdZB`BBT�Be`Bm�BjBr�Bx�Bz�B|�Bz�B�%B�7B�+B�JB�VB�bB�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�'B�'B�!B�3B�FB�LB�RB�FB�RB�XB�qB�wB�qB�dB�9B�qB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�BB�BB�;B�ZB�fB�B�B�B�B��B	B	B	+B	1B	DB	JB	\B	�B	�B	�B	�B	 �B	"�B	"�B	"�B	"�B	#�B	#�B	'�B	/B	33B	33B	49B	6FB	8RB	9XB	;dB	>wB	?}B	C�B	E�B	E�B	E�B	E�B	E�B	E�B	C�B	B�B	G�B	I�B	N�B	W
B	[#B	]/B	\)B	]/B	ZB	^5B	_;B	_;B	_;B	dZB	dZB	dZB	iyB	l�B	k�B	k�B	k�B	k�B	q�B	u�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�VB	�hB	�{B	�{B	�{B	�uB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�9B	�?B	�FB	�RB	�XB	�XB	�qB	�wB	�jB	�jB	ÖB	ÖB	ĜB	ĜB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�/B	�;B	�;B	�5B	�5B	�;B	�BB	�HB	�HB	�HB	�HB	�NB	�ZB	�`B	�`B	�ZB	�`B	�fB	�fB	�`B	�ZB	�TB	�TB	�`B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B
1B
1B
	7B
1B
	7B

=B
	7B
	7B
DB
DB
PB
\B
hB
oB
oB
hB
hB
oB
hB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
 �B
 �B
�B
 �B
"�B
!�B
"�B
$�B
$�B
#�B
$�B
$�B
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
(�B
)�B
)�B
)�B
(�B
'�B
-B
.B
/B
.B
.B
-B
-B
0!B
0!B
0!B
0!B
1'B
1'B
0!B
0!B
2-B
49B
7LB
6FB
5?B
49B
5?B
7LB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
<jB
=qB
<jB
<jB
;dB
:^B
=qB
=qB
>wB
>wB
>wB
?}B
>wB
?}B
@�B
@�B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
A�B
A�B
A�B
B�B
A�B
A�B
A�B
@�B
B�B
C�B
C�B
B�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
G�B
G�B
H�B
H�B
J�B
K�B
K�B
J�B
J�B
I�B
H�B
G�B
I�B
L�B
L�B
M�B
M�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
O�B
P�B
Q�B
R�B
R�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
P�B
S�B
T�B
VB
T�B
S�B
S�B
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
VB
VB
VB
W
B
VB
VB
VB
XB
XB
XB
XB
XB
YB
YB
YB
XB
XB
XB
XB
XB
XB
YB
YB
YB
W
B
W
B
YB
ZB
ZB
ZB
YB
ZB
ZB
ZB
\)B
]/B
]/B
]/B
\)B
^5B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
^5B
^5B
`BB
`BB
aHB
aHB
`BB
`BB
_;B
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
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
ffB
hsB
gmB
hsB
iyB
hsB
iyB
jB
jB
jB
jB
jB
iyB
jB
jB
iyB
jB
k�B
k�B
jB
jB
jB
jB
l�B
m�B
m�B
n�B
n�B
o�B
o�B
n�B
n�B
m�B
l�B
n�B
o�B
o�B
o�B
n�B
n�B
o�B
p�B
p�B
o�B
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
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
r�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bx�Bw�Bw�Bv�Bu�Bu�BtBr-BoBs�Br�Bo Bf�BS[BZ�BJ�BIBIlB)BjB��B�,B�kB��B��Bm�B�B��B�B{JB�-B}qB�BpUBjBXEBI�B49B	�B	�B+BdB�B�B�TB��B�lB�GB�!B�KB�HB�B�B��B�{BΥB�rB�mB��B��B�[B\�BS�BT�Bg�Bh�Bb�BZ�BIlB \B
�cB~B�B
�kB
��B
�0B
�B
�#B
�pB
��B
��B
kB
m�B
uB
o5B
cTB
RB
,B
4�B
+B
�B
�B
�B
B	�wB	�-B	�=B	�'B	ðB	��B	��B	��B	�1B	�OB	�OB	�
B	�KB	� B	��B	��B	�SB	�3B	��B	��B	�B	y�B	u�B	kB	_pB	V9B	S�B	c�B	d�B	b�B	[�B	G�B	2�B	=�B	6zB	5tB	/5B	5�B	8lB	7fB	4nB	-�B	"�B	�B	(B�<B	�B	�B	
�B	
�B�JB�>B��B�B�B�~B�YBٚB��B�%B�gBΥB��B�;B�'B��B��B�"B��B�@B�vB��B��B�)B�KB�RB�FB�B�,B�4B�'B��B�kB��B��B� B��B��Bz�B� B�4Bz^BwLB{�B~wB|jBr�BdtBb�Bb�Bd@BnBjBe,BZ�B_!Bd&B]BYBW�BM�BI7BQ�BT�BS�BIRBO�BG�BNVBMjBH�BD�B@�B>�B9�B>BB<6B<PB>B="B8RB5%B0�B$ZB(XB%FB&LB�B+6B-CB'�B�B%�B#�B&�B*eB"�BB�B'mB&�B!�B"�B&�B'�B&�B&�B)yB(XB%�B�B�B:B�B�B�B�B%B"hBkB�BtB��B9B)�B,=B+QB,=B)DB&LB#�B!|B �B�B�B�B$ZB+6B+QB&�B'RB(>B$�B%`B#�BpB+�B*�B.}B,�B.�B2�B:�B?}B?}BA�B>�B<B<BA�B@OBIBI7BOBBOBO(BKDBL0BMPBNpBS&BVBU2BU2BVBV9BVBS@BNpBNpBL�BL~BVmBZQBW�BPBV�BdtBb�B`Ba�Bd�BaBV�BfBnBkkBs3By$B{JB}�B|B��B��B��B��B��B��B��B��B��B��B��B��B��B�1B��B�B�B�B� B�B�&B�hB�mB�'B�[B�[B��B�hB�zB��B��B��B��B��B��B��B��B��B�?B�(B��B�B��B�B� B� B�B�.B�B�PB�PB�[B�KBچB�vB��BߤB��B��B��B��B��B�GB�`B	;B	SB	_B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	"�B	"�B	#B	#�B	$&B	($B	/5B	3hB	3�B	4nB	6`B	8lB	9�B	;�B	>�B	?�B	C�B	E�B	E�B	E�B	E�B	E�B	E�B	C�B	B�B	G�B	J=B	O\B	W?B	[=B	]IB	\]B	]dB	Z�B	^OB	_VB	_VB	_pB	dtB	d�B	d�B	i�B	l�B	k�B	k�B	k�B	k�B	q�B	vB	}B	�B	� B	�B	� B	� B	�AB	�GB	�SB	�mB	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�B	�0B	�=B	�B	�;B	�GB	�9B	�TB	�?B	�`B	�lB	��B	��B	��B	��B	��B	��B	ðB	ðB	��B	��B	��B	��B	��B	�B	� B	��B	�B	��B	�B	�B	�&B	�9B	�$B	�$B	�+B	�+B	�+B	�SB	�EB	�KB	�IB	�VB	�VB	�jB	�OB	�VB	�vB	�bB	�bB	�bB	�|B	�hB	�B	�zB	�zB	�tB	�zB	�fB	�B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	�B	�B	�B	��B	�B	��B	�B	�"B	�B	�B	�B
 B
 B
 B
GB
-B
GB
3B
-B
GB
9B
9B
9B
MB
?B
	7B
KB
fB
	RB
fB
	7B

XB
	lB
	�B
xB
�B
�B
vB
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
!�B
 �B
 �B
�B
 �B
"�B
!�B
#B
$�B
$�B
$&B
$�B
$�B
%�B
%�B
'B
'B
'B
(
B
(
B
($B
(�B
)B
)*B
*B
*B
*B
)DB
(XB
-)B
./B
/5B
./B
./B
-CB
-CB
0UB
0;B
0UB
0;B
1AB
1vB
0oB
0�B
2aB
4TB
72B
6`B
5ZB
4nB
5tB
7fB
8lB
8RB
8lB
8�B
8�B
9�B
:xB
<jB
=qB
<�B
<jB
;B
:�B
=�B
=�B
>�B
>�B
>�B
?�B
>�B
?�B
@�B
@�B
?�B
?�B
@�B
A�B
A�B
A�B
A�B
B�B
A�B
A�B
A�B
BuB
A�B
A�B
A�B
@�B
B�B
C�B
C�B
B�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
E�B
G�B
G�B
H�B
H�B
J�B
K�B
K�B
J�B
J�B
I�B
H�B
G�B
I�B
L�B
L�B
M�B
M�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
PB
O�B
P�B
P�B
Q B
P�B
Q B
Q B
O�B
Q B
Q�B
R�B
R�B
RB
RB
RB
RB
SB
RB
QB
S�B
T�B
VB
UB
TB
TB
VB
VB
VB
W
B
W
B
W$B
VB
VB
VB
VB
W
B
VB
VB
VB
XB
XB
W�B
XB
XB
YB
YB
YB
X+B
X+B
X+B
XEB
XEB
X+B
YB
YB
YB
W?B
W?B
Y1B
Z7B
Z7B
Z7B
YKB
Z7B
Z7B
ZQB
\CB
]IB
]/B
]IB
\CB
^5B
]IB
]IB
]IB
]IB
^jB
_VB
_;B
^OB
^OB
`\B
`\B
aHB
aHB
`BB
`\B
_pB
abB
abB
abB
bhB
bhB
bhB
c:B
cnB
cnB
dZB
dZB
cnB
c�B
cnB
cnB
cnB
cnB
dtB
ezB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
ffB
h�B
g�B
h�B
i�B
hsB
i�B
jeB
jB
jB
j�B
jB
i�B
j�B
j�B
i�B
j�B
k�B
k�B
j�B
j�B
j�B
j�B
l�B
m�B
m�B
n�B
n�B
o�B
o�B
n}B
n�B
m�B
l�B
n}B
o�B
o�B
o�B
n�B
n�B
o�B
p�B
p�B
o�B
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
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
r�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<F?<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808130034372018081300343720180813003437201808130200162018081302001620180813020016201808140025462018081400254620180814002546  JA  ARFMdecpA19c                                                                20180809093517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180809003518  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180809003521  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180809003522  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180809003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180809003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180809003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180809003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180809003523  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180809003523                      G�O�G�O�G�O�                JA  ARUP                                                                        20180809005638                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180809153406  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20180812153437  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180812153437  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180812170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180813152546  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                