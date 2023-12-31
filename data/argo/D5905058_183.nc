CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-10-21T03:37:48Z creation;2019-10-21T03:37:53Z conversion to V3.1;2023-06-29T05:50:53Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ά   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191021033748  20230705031506  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_183                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @��aG� 1   @��333 @6�-�b���ڹ�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D��3D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D��3D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @`  @�  @�  A  A(  AI��Ah  A�  A�  A�  A�  A�  A�  A�  A�  B  B
  B  B  B"  B*  B2  B:  BB  BJ  BR  BZ  Bb  Bj  Br  Bz  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C � C� CffC� C� C
� C� C� C� C� C� C� C� C� C� C� C � C"� C$� C&� C(� C*� C,� C.� C0� C2� C4� C6� C8� C:� C<� C>� C@� CB� CD� CF� CH� CJ� CL� CN� CP� CR� CT� CV� CX� CZ� C\� C^� C`� Cb� Cd� Cf� Ch� Cj� Cl� Cn� Cp� Cr� Ct� Cv� Cx� Cz� C|� C~� C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�33C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�@ C�@ C�@ C�@ D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^&fD^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D��3D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�L�D���D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�S3D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D D�� D� D�P DÐ D�� D� D�P DĐ D�� D� D�P DŐ D�� D� D�P DƐ D�� D� D�P Dǐ D�� D� D�P DȐ D�� D� D�P Dɐ D�� D� D�P Dʐ D�� D� D�P Dː D�� D� D�P D̐ D��3D� D�P D͐ D�� D� D�P Dΐ D�� D� D�P Dϐ D�� D� D�P DА D�� D� D�P Dѐ D�� D� D�P DҐ D�� D� D�P DӐ D�� D� D�P DԐ D��3D� D�P DՐ D�� D� D�P D֐ D�� D� D�P Dא D�� D� D�P Dؐ D�� D� D�P Dِ D�� D� D�P Dڐ D�� D� D�P Dې D�� D� D�P Dܐ D�� D� D�P Dݐ D�� D� D�P Dސ D�� D� D�P Dߐ D�� D� D�P D�� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D� D�P D�� D�� D�3D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aӛ�Aӗ�Aә�Aә�Aә�Aә�Aӛ�Aӝ�Aә�Aә�Aӕ�AӓuAӍPAӋDAӉ7AӋDAӋDAӋDAӋDAӋDAӋDAӋDAӉ7AӉ7AӅA�z�A�l�A�hsA���A�"�A�~�A�hsA��A�t�A��A��A��^A��A�ĜA���A�|�A�l�A�~�A�S�A�9XA��yA��
A��A���A��A�"�A��mA��A��A�x�A�(�A��A��A���A���A���A�K�A�bA���A���A�{A�
=A��A���A���A�(�A���A��RA���A�Q�A�A���A� �A���A��DA�VA��A�  A�&�A��mA���A���A��A�n�A�VA���A�bA�=qA�hsA�"�A���A���A�A��A���A�ZA�|�A�/A���A��A��A��\A��9A���A�VA��DA���A�9XA�C�A��A�E�A��A��/A�JA}dZAzE�AxZAwC�Av9XAu\)Ar�ApffAoK�AmhsAj�!Ag\)Ae�-Aa;dA_?}A]�A\��A[dZAZbAWVAS+ANĜAL�`AJ�AHz�AF�jAE%AD �AC�FAB�A@^5A>�/A=�wA<$�A;�A9��A9+A8��A7�
A7XA6��A6�\A4z�A2��A1x�A0�9A0  A/�A-��A-�A+�-A(�`A%?}A$~�A$A�A%A"��A"  A"VA!ƨA ��A =qA�#A�AQ�A�#A��A�A�yA�+AJAhsA��A-A�FAp�A�/A�+A\)AA�A\)A��AG�AI�A\)A	�
A�`A�#A%An�A�mAt�A��A5?Ar�A��A�A��A&�A��A&�@���@�hs@��P@�ff@�X@���@���@��@�p�@�%@�@�^5@�z�@�A�@�  @��@�G�@�  @�;d@��@�p�@���@�O�@�b@��@��@��#@�G�@��/@���@���@�b@�@ܬ@�;d@�ƨ@�ff@��@ޏ\@ݡ�@�A�@�hs@�=q@؛�@�l�@ڸR@�n�@�M�@ם�@�{@��`@�p�@�{@�-@ɩ�@�ȴ@�33@�l�@�b@��@Ə\@�V@�^5@��T@ũ�@��T@�=q@�l�@Ə\@��#@�hs@��`@���@�o@�o@���@�n�@��T@�p�@�V@�I�@��m@��P@�
=@��+@��@��9@��@���@���@��`@�@�O�@��/@�r�@���@�|�@��@���@���@�n�@�{@��^@���@�%@���@��u@��D@��D@��@��j@��9@���@�Z@�ȴ@�M�@�E�@���@�I�@�C�@���@�9X@�n�@��-@�hs@��@�A�@��@��#@�%@�r�@���@�|�@�@��y@�V@���@�X@��@���@��@��;@�dZ@��H@�M�@��h@�G�@�&�@���@���@�z�@�A�@� �@�1@�  @���@�C�@��y@��!@�~�@��+@���@���@���@�^5@��@��@�{@��@���@�p�@�G�@�7L@��@�V@��@��@�V@�V@�V@��`@��9@��@��@�I�@�1@��@�ƨ@���@���@���@�\)@�"�@�@��y@���@�5?@��@���@��@�O�@��@��@��j@��@�j@�(�@��
@�|�@�"�@�ȴ@�E�@�$�@��@���@�@���@�hs@�O�@�&�@���@��@��/@��j@��@��@�j@�I�@�ƨ@��P@�t�@�K�@��H@��\@�E�@�{@���@���@�x�@�?}@��/@��@�9X@�1'@� �@��@�1@��m@��@��P@�S�@���@���@���@��\@�M�@�$�@�J@��@���@�7L@�%@���@��9@��@�(�@�ƨ@�K�@���@��y@���@���@���@��\@�~�@�~�@�V@�5?@��@�?}@�&�@�V@���@��`@���@�r�@�j@�I�@�9X@�(�@�;@�@~�y@~v�@}��@}V@|�@|��@|Z@{��@{�@{�@{t�@{"�@{@z�!@z�@y��@yG�@x�`@xr�@w��@wK�@w
=@vȴ@vff@v@uO�@t�@tz�@sƨ@s��@s�@sdZ@r~�@q�#@q�^@qx�@q�@p�9@p�@pr�@pQ�@p �@o�;@o;d@n��@n�+@n@mp�@l�@l��@lI�@k�m@k��@kƨ@k�@j�@j�!@j-@i�@i��@ihs@h�`@h��@h��@hr�@hA�@g�;@g�@gK�@f��@fff@e�@e�h@eO�@eV@d��@dj@d�@c��@c"�@b�@b�!@b�\@bM�@bJ@a��@ahs@`�`@_�@_l�@_K�@_
=@^�@^ff@]�@]�@]@]�h@]�@]/@\�/@\�D@[�m@[��@[S�@Z�@Z��@Z��@Z�\@Z~�@ZM�@ZJ@Y��@Y7L@XĜ@XbN@X1'@X �@W�@W
=@Vff@V5?@V{@U�T@U�@UV@T�@T�@TZ@T�@T�@S��@S�@SC�@S@R�\@R=q@Q�@Qx�@Q%@P�u@P1'@O�@O;d@N��@N5?@M��@M?}@M/@M�@L��@L�@L�@K��@K�
@K�F@KS�@K@J�@J��@J��@J~�@J�@I�@I��@I�7@H�`@G�;@G�@G|�@G\)@F��@Fv�@Fv�@Fv�@F@Ep�@E?}@E�@D��@D�/@D�j@D��@Dj@D(�@C�
@C�F@CdZ@C33@B��@A��@Ax�@A7L@@��@@�u@@b@?�@?��@?��@>�y@>��@>V@>$�@=�@=��@=@=p�@=�@<��@<(�@;�m@;��@;S�@;o@:�@:��@:~�@:M�@:=q@9��@9hs@97L@9�@8��@8�9@8�u@8�@8�@8bN@8 �@8  @7�w@7�P@7|�@7�@6��@6�@6�R@6�R@6�+@6v�@6V@6@5�-@5�@5/@4��@4z�@41@3dZ@3o@2�@2��@2n�@2J@1��@1�^@1��@17L@0Ĝ@0��@0�u@0bN@01'@01'@01'@/�;@/��@/|�@/+@.�y@.��@.V@-�@-@-�@-�@,�j@,�D@,Z@,9X@,1@+�m@+��@+S�@+C�@+33@+o@*��@*��@*�\@*~�@*~�@*~�@*�@)��@)��@)7L@)�@)%@(Ĝ@(�u@(A�@(1'@(  @'�;@'��@'K�@'�@&�@&�+@%�T@%��@%`B@$��@$�@$�@$j@$1@#�m@#��@#t�@#dZ@#dZ@#dZ@#C�@#@"��@"M�@"J@!��@!�^@!�7@!X@!G�@!�@ Ĝ@ ��@ �@ A�@ b@   @�@�;@�P@\)@;d@��@��@V@$�@��@O�@/@�@��@�@z�@��@�F@��@��@�@dZ@S�@"�@o@@�@��@�!@^5@J@�@��@�7@X@G�@�@%@��@��@��@��@�@�@bN@1'@�@��@�w@�P@|�@l�@+@
=@�R@�+@V@E�@E�@�T@��@p�@O�@�@V@�@�@�D@(�@�@1@ƨ@��@��@dZ@C�@�H@��@~�@^5@�@��@�^@��@��@��@�7@X@�@��@�@�@r�@Q�@b@  @�@�w@|�@\)@;d@+@�@�y@�R@��@��@�+@v�@V@5?@$�@�@@��@p�@?}@�@�@�j@�@��@z�@Z@Z@9X@1@�m@�m@�
@�
@�
@�
@�F@��@�@dZ@S�@"�@
�@
��@
��@
^5@
�@	��@	��@
J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aӛ�Aӗ�Aә�Aә�Aә�Aә�Aӛ�Aӝ�Aә�Aә�Aӕ�AӓuAӍPAӋDAӉ7AӋDAӋDAӋDAӋDAӋDAӋDAӋDAӉ7AӉ7AӅA�z�A�l�A�hsA���A�"�A�~�A�hsA��A�t�A��A��A��^A��A�ĜA���A�|�A�l�A�~�A�S�A�9XA��yA��
A��A���A��A�"�A��mA��A��A�x�A�(�A��A��A���A���A���A�K�A�bA���A���A�{A�
=A��A���A���A�(�A���A��RA���A�Q�A�A���A� �A���A��DA�VA��A�  A�&�A��mA���A���A��A�n�A�VA���A�bA�=qA�hsA�"�A���A���A�A��A���A�ZA�|�A�/A���A��A��A��\A��9A���A�VA��DA���A�9XA�C�A��A�E�A��A��/A�JA}dZAzE�AxZAwC�Av9XAu\)Ar�ApffAoK�AmhsAj�!Ag\)Ae�-Aa;dA_?}A]�A\��A[dZAZbAWVAS+ANĜAL�`AJ�AHz�AF�jAE%AD �AC�FAB�A@^5A>�/A=�wA<$�A;�A9��A9+A8��A7�
A7XA6��A6�\A4z�A2��A1x�A0�9A0  A/�A-��A-�A+�-A(�`A%?}A$~�A$A�A%A"��A"  A"VA!ƨA ��A =qA�#A�AQ�A�#A��A�A�yA�+AJAhsA��A-A�FAp�A�/A�+A\)AA�A\)A��AG�AI�A\)A	�
A�`A�#A%An�A�mAt�A��A5?Ar�A��A�A��A&�A��A&�@���@�hs@��P@�ff@�X@���@���@��@�p�@�%@�@�^5@�z�@�A�@�  @��@�G�@�  @�;d@��@�p�@���@�O�@�b@��@��@��#@�G�@��/@���@���@�b@�@ܬ@�;d@�ƨ@�ff@��@ޏ\@ݡ�@�A�@�hs@�=q@؛�@�l�@ڸR@�n�@�M�@ם�@�{@��`@�p�@�{@�-@ɩ�@�ȴ@�33@�l�@�b@��@Ə\@�V@�^5@��T@ũ�@��T@�=q@�l�@Ə\@��#@�hs@��`@���@�o@�o@���@�n�@��T@�p�@�V@�I�@��m@��P@�
=@��+@��@��9@��@���@���@��`@�@�O�@��/@�r�@���@�|�@��@���@���@�n�@�{@��^@���@�%@���@��u@��D@��D@��@��j@��9@���@�Z@�ȴ@�M�@�E�@���@�I�@�C�@���@�9X@�n�@��-@�hs@��@�A�@��@��#@�%@�r�@���@�|�@�@��y@�V@���@�X@��@���@��@��;@�dZ@��H@�M�@��h@�G�@�&�@���@���@�z�@�A�@� �@�1@�  @���@�C�@��y@��!@�~�@��+@���@���@���@�^5@��@��@�{@��@���@�p�@�G�@�7L@��@�V@��@��@�V@�V@�V@��`@��9@��@��@�I�@�1@��@�ƨ@���@���@���@�\)@�"�@�@��y@���@�5?@��@���@��@�O�@��@��@��j@��@�j@�(�@��
@�|�@�"�@�ȴ@�E�@�$�@��@���@�@���@�hs@�O�@�&�@���@��@��/@��j@��@��@�j@�I�@�ƨ@��P@�t�@�K�@��H@��\@�E�@�{@���@���@�x�@�?}@��/@��@�9X@�1'@� �@��@�1@��m@��@��P@�S�@���@���@���@��\@�M�@�$�@�J@��@���@�7L@�%@���@��9@��@�(�@�ƨ@�K�@���@��y@���@���@���@��\@�~�@�~�@�V@�5?@��@�?}@�&�@�V@���@��`@���@�r�@�j@�I�@�9X@�(�@�;@�@~�y@~v�@}��@}V@|�@|��@|Z@{��@{�@{�@{t�@{"�@{@z�!@z�@y��@yG�@x�`@xr�@w��@wK�@w
=@vȴ@vff@v@uO�@t�@tz�@sƨ@s��@s�@sdZ@r~�@q�#@q�^@qx�@q�@p�9@p�@pr�@pQ�@p �@o�;@o;d@n��@n�+@n@mp�@l�@l��@lI�@k�m@k��@kƨ@k�@j�@j�!@j-@i�@i��@ihs@h�`@h��@h��@hr�@hA�@g�;@g�@gK�@f��@fff@e�@e�h@eO�@eV@d��@dj@d�@c��@c"�@b�@b�!@b�\@bM�@bJ@a��@ahs@`�`@_�@_l�@_K�@_
=@^�@^ff@]�@]�@]@]�h@]�@]/@\�/@\�D@[�m@[��@[S�@Z�@Z��@Z��@Z�\@Z~�@ZM�@ZJ@Y��@Y7L@XĜ@XbN@X1'@X �@W�@W
=@Vff@V5?@V{@U�T@U�@UV@T�@T�@TZ@T�@T�@S��@S�@SC�@S@R�\@R=q@Q�@Qx�@Q%@P�u@P1'@O�@O;d@N��@N5?@M��@M?}@M/@M�@L��@L�@L�@K��@K�
@K�F@KS�@K@J�@J��@J��@J~�@J�@I�@I��@I�7@H�`@G�;@G�@G|�@G\)@F��@Fv�@Fv�@Fv�@F@Ep�@E?}@E�@D��@D�/@D�j@D��@Dj@D(�@C�
@C�F@CdZ@C33@B��@A��@Ax�@A7L@@��@@�u@@b@?�@?��@?��@>�y@>��@>V@>$�@=�@=��@=@=p�@=�@<��@<(�@;�m@;��@;S�@;o@:�@:��@:~�@:M�@:=q@9��@9hs@97L@9�@8��@8�9@8�u@8�@8�@8bN@8 �@8  @7�w@7�P@7|�@7�@6��@6�@6�R@6�R@6�+@6v�@6V@6@5�-@5�@5/@4��@4z�@41@3dZ@3o@2�@2��@2n�@2J@1��@1�^@1��@17L@0Ĝ@0��@0�u@0bN@01'@01'@01'@/�;@/��@/|�@/+@.�y@.��@.V@-�@-@-�@-�@,�j@,�D@,Z@,9X@,1@+�m@+��@+S�@+C�@+33@+o@*��@*��@*�\@*~�@*~�@*~�@*�@)��@)��@)7L@)�@)%@(Ĝ@(�u@(A�@(1'@(  @'�;@'��@'K�@'�@&�@&�+@%�T@%��@%`B@$��@$�@$�@$j@$1@#�m@#��@#t�@#dZ@#dZ@#dZ@#C�@#@"��@"M�@"J@!��@!�^@!�7@!X@!G�@!�@ Ĝ@ ��@ �@ A�@ b@   @�@�;@�P@\)@;d@��@��@V@$�@��@O�@/@�@��@�@z�@��@�F@��@��@�@dZ@S�@"�@o@@�@��@�!@^5@J@�@��@�7@X@G�@�@%@��@��@��@��@�@�@bN@1'@�@��@�w@�P@|�@l�@+@
=@�R@�+@V@E�@E�@�T@��@p�@O�@�@V@�@�@�D@(�@�@1@ƨ@��@��@dZ@C�@�H@��@~�@^5@�@��@�^@��@��@��@�7@X@�@��@�@�@r�@Q�@b@  @�@�w@|�@\)@;d@+@�@�y@�R@��@��@�+@v�@V@5?@$�@�@@��@p�@?}@�@�@�j@�@��@z�@Z@Z@9X@1@�m@�m@�
@�
@�
@�
@�F@��@�@dZ@S�@"�@
�@
��@
��@
^5@
�@	��@	��@
J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B/B/B/B.B.B.B/B.B/B0!B1'B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B2-B1'B0!B+B"�B[#BO�BN�BffB�B�+B��B��B�qB��B��B��B�B�fB��BBBDB�B#�B(�B,B:^B<jB;dB?}BJ�BW
B]/B^5BZBZB]/BiyBbNBdZBe`BcTBe`BffBffBe`BbNB[#BT�BS�BJ�BB�B@�B@�BA�B:^B1'B+B,B)�B!�B�B\B1BB  B��B�B��B�LB��B~�BcTB8RB�B	7BB
��B
�B
�NB
�B
��B
�?B
�B
��B
��B
�VB
�B
p�B
gmB
[#B
Q�B
C�B
2-B
�B
DB
%B	��B	��B	�B	��B	��B	�qB	�B	�{B	�7B	q�B	aHB	T�B	O�B	E�B	<jB	+B	{B�B�`B�B��BĜB�^B�?B�3B�B��B��B��B�uB��B�{B�hB�bB�VB�DB�7B�1B~�Bs�BgmBcTBaHB_;B_;B\)B\)BQ�BE�BA�B@�B\)BM�BO�BiyBjBk�Bo�Bt�Bq�Bn�Bk�BiyBe`B^5B]/B[#BVBVBR�BS�BM�BM�BL�BK�BK�BL�BR�BT�BP�BJ�BA�B?}B?}B@�B?}BD�BI�BW
Bo�B� B� B� B� B~�B�Bz�Br�Bo�BffBiyBffBjBiyBk�Bp�By�B�B� Bv�B|�B�B�B}�Bz�B�B�B�B�B�JB��B�!B��B��B��B��B��B�\B�bB�VB�DB�JB�hB�B�FB�FB�?B�9B�B��B�FB��B��B��B��B��BĜB��B�dB�-B��B��B��B��B�B�RBÖBBBƨBǮBǮB��B��B�/B�/B�/B�/B�)B�BB�ZB�yB�B�B�B�yB�yB�B�B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	DB	PB	bB	hB	uB	�B	�B	�B	�B	�B	%�B	&�B	&�B	'�B	)�B	'�B	'�B	$�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	+B	-B	/B	1'B	33B	33B	49B	9XB	:^B	9XB	=qB	?}B	A�B	D�B	E�B	H�B	J�B	M�B	N�B	N�B	T�B	YB	\)B	`BB	aHB	bNB	cTB	dZB	dZB	gmB	hsB	hsB	hsB	hsB	hsB	jB	n�B	p�B	t�B	u�B	v�B	w�B	x�B	x�B	x�B	y�B	z�B	{�B	|�B	~�B	�B	�B	�B	�B	�%B	�1B	�DB	�PB	�VB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�3B	�9B	�?B	�FB	�FB	�LB	�RB	�RB	�XB	�^B	�^B	�^B	�dB	�jB	�}B	��B	��B	B	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�TB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B
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
VB
VB
bB
bB
bB
hB
hB
oB
oB
oB
oB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
!�B
"�B
"�B
"�B
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
%�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
.B
.B
.B
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
33B
33B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
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
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
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
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
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
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
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
XB
XB
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
[#B
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
^5B
^5B
^5B
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
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
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
jB
jB
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
k�B
k�B
l�B
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
m�B
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
o�B
o�B
o�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B/ B.�B.�B-�B-�B-�B/ B-�B.�B/�B1B2B2B2B2B2B1�B2B1�B2B1�B2B2B2B2GB2aB1�B2|B0�B2|B^�BRoBP�Bi*B��B��B��B��B��B�B��B� BܒB��B�wBGB�B�B�B%,B)�B-�B;0B<�B<jBA�BL�BW�B^5B^�BZ�B[=B_�Bk�Bc�BfLBf�Bc�Be�BgmBg�Bg8Bc�B\)BVBVBK�BCGBA;BA�BC�B<6B2�B+�B-B+�B# B�BHB	RB�B�B��B�IB��B�DB�:B�Bg�B:�BKB	�BAB
�6B
�B
�B
��B
��B
��B
�=B
�nB
�+B
��B
��B
rGB
iB
\�B
TB
F�B
5ZB
�B
�B
_B
 �B	��B	��B	յB	�vB	��B	��B	�YB	��B	s�B	cB	VB	Q�B	G�B	@iB	/�B	�B�MB�$B�xB��B�YB�dB�+B�%B� B��B�B�7B��B��B�B� B�B��B��B�=B��B��Bu?BhXBdZBb�B`�B`vB^�B_�BUMBFYBA�B@iB^BN�BO�BjBkkBl=Bp;Bu�BraBo�Bm]Bk�Bf�B^�B]�B[�BV�BV�BTBVBN�BN�BN<BMBL�BM�BT{BV9BR:BL~BB�B@�B@OBA B@ BD�BIRBV�Bo�B��B�B�B��B�B�B|PBtBp�BgBjBg8BkBi�Bk�BqBz�B��B��Bv�B}"B��B��B~�B{dB�aB��B��B��B�^B��B��B�ZB�B�B��B�sB��B� B�vB��B�^B�.B�}B�zB��B�+B��B� B�nB��B��B��B�BB�NB��BżBðB��B�9B��B��B��B��B��B��B�{B�uB�[B��BǔB�_B�DB�HB�~B�dB�IB�~BܬB��B�&B�B�B�B�B�B�B��B��B��B�%B�nB��B�B�aB�3B�3B�B�IB�B��B�B��B��B��B��B��B��B��B��B	 OB	)B	PB	.B	4B	&B	YB	B	�B	�B	 vB	%�B	&�B	'RB	(�B	*B	(�B	(�B	%�B	 �B	�B	�B	)B	�B	�B	B	B	�B	�B	�B	 �B	#B	+B	-)B	/5B	1[B	3hB	33B	4TB	9rB	:xB	9�B	=qB	?}B	AoB	D�B	E�B	H�B	J�B	M�B	N�B	OB	UB	YB	\B	`B	`�B	a�B	c B	d@B	d@B	gmB	hXB	hXB	hXB	hsB	hXB	jeB	n}B	poB	t�B	utB	vzB	w�B	x�B	x�B	x�B	y�B	z�B	{�B	|�B	~�B	��B	��B	��B	��B	��B	�B	�)B	�B	�<B	�pB	�}B	�TB	�uB	�gB	�mB	�sB	��B	��B	�xB	��B	��B	��B	��B	��B	��B	�B	� B	�B	�B	�B	�B	�%B	�B	�+B	�2B	�8B	�8B	�$B	�*B	�DB	�DB	�JB	�jB	�}B	�iB	�oB	B	ĜB	ƎB	ȚB	ɠB	��B	��B	̳B	��B	��B	��B	ҽB	ҽB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�#B	��B	�B	�/B	�OB	�;B	�BB	�'B	�BB	�-B	�bB	�hB	�TB	�FB	�LB	�LB	�8B	�8B	�XB	�>B	�>B	�sB	�yB	�B	�qB	�qB	�]B	�]B	�]B	�B	�iB	�B	�iB	��B	��B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
 �B
 �B
 �B
�B
�B
�B
�B
 B
�B
�B
�B
�B
B
B
�B
�B
B
�B
B
	B
	B
	B

#B

=B
)B
0B
B
B
B
B
VB
"B
<B
"B
"B
"B
VB
.B
.B
.B
NB
hB
TB
oB
TB
oB
[B
uB
aB
aB
FB
aB
MB
�B
mB
mB
mB
SB
mB
SB
SB
�B
mB
�B
mB
sB
sB
_B
yB
eB
kB
kB
kB
qB
qB
�B
�B
�B
~B
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
"�B
"�B
"�B
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
%�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
+B
*�B
+�B
+�B
+�B
,B
+�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
/B
/B
0!B
0B
1B
0�B
1B
1'B
1�B
2B
2-B
2-B
3B
3B
2�B
2�B
3B
3B
3B
3B
4B
4B
4B
4B
5%B
5ZB
6FB
6+B
72B
72B
72B
88B
8B
8B
88B
9$B
:*B
:DB
:DB
:DB
:*B
:DB
;JB
;JB
;dB
<PB
<PB
=<B
=VB
=<B
=VB
>BB
>]B
>]B
>]B
?HB
?HB
?HB
?cB
?cB
@OB
@4B
@OB
@iB
@iB
@iB
@iB
AUB
AUB
AoB
AoB
BuB
B[B
BAB
B[B
B[B
B[B
BuB
C{B
CaB
CaB
C�B
D�B
D�B
D�B
EmB
E�B
EmB
E�B
FtB
F�B
GzB
F�B
G�B
GzB
GzB
GzB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
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
L~B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
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
U�B
U�B
U�B
V�B
V�B
V�B
V�B
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
W�B
W�B
X�B
X�B
YB
Y�B
ZB
Y�B
ZB
ZB
ZB
Z�B
Z�B
Z�B
Z�B
[	B
Z�B
[	B
[�B
[�B
\B
[�B
[�B
\B
[�B
]B
]B
]B
]B
]B
]B
]�B
^B
^B
^B
^B
^B
]�B
^B
^B
_B
_!B
_B
_!B
`B
`B
`'B
`'B
`'B
aB
aB
`�B
a-B
a-B
b4B
bB
bB
b4B
bB
bB
c:B
c B
c:B
d&B
d&B
d@B
d@B
d@B
d@B
d@B
d@B
eFB
eFB
e,B
f2B
fLB
f2B
fB
f2B
gB
g8B
gRB
gRB
gRB
h$B
h>B
h>B
h>B
h>B
i*B
i_B
iDB
iDB
iDB
j0B
jKB
jKB
jeB
jKB
kQB
k6B
k6B
kQB
kkB
kkB
kQB
kkB
kkB
kkB
lWB
lWB
lWB
lqB
lqB
lWB
lWB
mwB
m]B
m]B
mwB
m]B
m]B
m]B
mCB
mCB
mCB
mCB
mwB
mCB
m]B
ncB
n}B
ncB
ncB
n}B
n}B
n}B
o�B
oOB
oiB
oOB
oi11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<|Q�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.5(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201910260036382019102600363820191026003638202306231718302023062317183020230623171830201910300226492019103002264920191030022649  JA  ARFMdecpA19c                                                                20191021123724  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191021033748  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20191021033751  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191021033751  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191021033752  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20191021033752  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191021033752  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20191021033752  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191021033753  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191021033753                      G�O�G�O�G�O�                JA  ARUP                                                                        20191021035412                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20191021153556  CV  JULD            G�O�G�O�F�-�                JM  ARCAJMQC2.0                                                                 20191025153638  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191025153638  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20191029172649  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081830  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031506                      G�O�G�O�G�O�                