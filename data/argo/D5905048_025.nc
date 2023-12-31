CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-07T00:35:17Z creation;2016-08-07T00:35:19Z conversion to V3.1;2019-12-19T08:29:56Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20160807003517  20200116201516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_025                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��6J%+ 1   @��7��� @3����d�u%F
�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�3D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @}p�@��@��A�\A:�\AZ�\Az�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#�\C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?�\CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D j=D �=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=D	j=D	�=D
j=D
�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=D j=D �=D!j=D!�=D"j=D"�=D#j=D#�=D$j=D$�=D%j=D%�=D&j=D&�=D'j=D'�=D(j=D(�=D)j=D)�=D*j=D*�=D+j=D+�=D,j=D,�=D-j=D-�=D.j=D.�=D/j=D/�=D0j=D0�=D1j=D1�=D2j=D2�=D3j=D3�=D4j=D4�=D5j=D5�=D6j=D6�=D7j=D7�=D8j=D8�=D9j=D9�=D:j=D:�=D;j=D;�=D<j=D<�=D=j=D=�=D>j=D>�=D?j=D?�=D@j=D@�=DAj=DA�=DBj=DB�=DCj=DC�=DDj=DD�=DEj=DE�=DFj=DF�=DGj=DG�=DHj=DH�=DIj=DI�=DJj=DJ�=DKj=DK�=DLj=DL�=DMj=DM�=DNj=DN�=DOj=DO�=DPj=DP�=DQj=DQ�=DRj=DR�=DSj=DS�=DTj=DT�=DUj=DU�=DVj=DV�=DWj=DW�=DXj=DX�=DYj=DY�=DZj=DZ�=D[j=D[�=D\j=D\�=D]j=D]�=D^j=D^�=D_j=D_��D`j=D`�=Daj=Da�=Dbj=Db�=Dcj=Dc�=Ddj=Dd�=Dej=De�=Dfj=Df�=Dgj=Dg�=Dhj=Dh�=Dij=Di�=Djj=Dj�=Dkj=Dk�=Dlj=Dl�=Dmj=Dm�=Dnj=Dn�=Doj=Do�Dpj=Dp�=Dqj=Dq�=Drj=Dr�=Dsj=Ds�=Dtj=Dt�=Duj=Du�=Dvj=Dv�=Dwj=Dw�=Dxj=Dx�=Dyj=Dy�=Dzj=Dz�=D{j=D{�=D|j=D|�=D}j=D}�=D~j=D~�=Dj=D�=D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�1�D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uDµD��D�5D�uDõD��D�5D�uDĵD��D�5D�uDŵD��D�5D�uDƵD��D�5D�uDǵD��D�5D�uDȵD��D�5D�uDɵD��D�5D�uDʵD��D�5D�uD˵D��D�5D�uD̵D��D�5D�uD͵D��D�5D�uDεD��D�5D�uDϵD��D�5D�uDеD��D�5D�uDѵD��D�5D�uDҵD��D�5D�uDӵD��D�5D�uDԵD��D�5D�uDյD��D�5D�uDֵD��D�5D�uD׵D��D�5D�uDصD��D�5D�uDٵD��D�5D�uDڵD��D�5D�uD۵D��D�5D�uDܵD��D�5D�uDݵD��D�5D�uD޵D��D�5D�uDߵD��D�5D�uD�D��D�5D�uD�D��RD�5D�uD�D���D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D���D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD��D��D�5D�uD�D���D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�xRD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�!A�9A�RA�A��A�A��A��A��A��A��A���A��A��A���A���A���A��\A��A��A�z�A�l�A�\)A�(�A���A��`A܋DA�K�A�t�A�/A��A���A�ȴA�M�AՅA��AԁA�VA�5?A��AЗ�A�jAω7A�O�A��
A�dZA�S�A�(�A���A�7LAŝ�A�ZA���A��TA���A�VA�=qA��A�A��yA���A�v�A�A�A��A��PA�A��A���A��A�(�A�\)A��RA�p�A�ȴA�A��mA�l�A�{A��7A��A��hA�/A�A�r�A�?}A�`BA�XA� �A��FA�\)A�XA�ȴA�{A��A��A�  A�&�A�$�A��9A��A��^A��mA�7LA��A�A��uA�dZA��A{O�AyhsAx�uAw�7Au%Aq+Al��Ai��AeAd$�Ab��Ab=qA`Q�A]O�A\�A\ �A[�^A[%AZ~�AZAV�/ATbAR�jAQ|�AO��AN=qAM+AL5?AK%AI��AHffAG�AFAEC�AD��AC�AB��AA�A?�7A>�A=�#A=33A<~�A;S�A:ĜA9�mA9"�A8(�A6(�A3t�A2ĜA2 �A1C�A/t�A.��A.5?A-oA,�A)�TA(�uA(=qA'��A'��A'"�A$ffA"�!A!|�A �jA v�A�^AJAO�A�jA��AdZA�DA�A��A�AJAoA��AƨAbNA33A��A�7A"�A5?A��A
ffA	�hA	+A9XAl�A��At�A�!A�PA"�AA�#A ��A �!A n�@�\)@��@���@� �@��P@�^5@���@��;@��@�ff@�O�@��@���@�ff@��@�E�@�@�+@�^5@�5?@��#@蛦@�bN@�K�@�{@�w@�h@��u@��
@ߍP@��@�@܋D@�ƨ@��@�ff@�M�@�$�@�Ĝ@׮@��H@�5?@���@�X@�bN@�;d@�~�@�J@�Ĝ@θR@�=q@��@��@���@͉7@�r�@�C�@���@ȴ9@�1@�dZ@�C�@���@�v�@�M�@���@�%@ċD@��m@�"�@��T@��/@�Z@�n�@�@���@�9X@��
@�C�@��@��@�33@��/@�r�@��F@��H@�$�@�`B@���@�bN@�ƨ@��H@�v�@�=q@�hs@��@��@�"�@��!@�t�@��R@�5?@���@���@�Ĝ@�\)@�-@��@��@���@��@��@�-@���@���@�5?@���@���@���@��7@�J@��\@�M�@�G�@��F@�E�@�5?@�5?@���@�t�@���@��u@�/@��@��;@�"�@�
=@���@���@���@��\@�$�@�V@���@�r�@���@��@��H@��@�C�@�S�@���@���@�{@�$�@���@�V@�E�@�$�@��@���@�G�@��9@� �@�ƨ@�|�@�dZ@�l�@�|�@�dZ@�S�@�S�@�K�@�33@��@���@�ȴ@��7@�Ĝ@��D@�A�@���@���@���@���@���@�dZ@�"�@�M�@���@��@���@��@�-@��!@���@���@��\@�~�@�{@���@���@�X@�7L@�V@��j@�I�@�9X@� �@�  @��@���@�C�@��R@�~�@�n�@�E�@�$�@�@���@�?}@��@�%@���@���@���@�@���@��+@�~�@�V@�E�@�-@��T@�@��-@���@�`B@�?}@�/@��@�bN@�1'@��@��
@��w@�|�@�33@��y@��@���@�n�@�V@�@��@��^@���@�hs@�%@�Ĝ@���@��@�Q�@�9X@�b@�t�@�ȴ@�^5@���@���@�7L@��@���@��/@���@��@��P@�|�@�33@��y@�V@���@��@���@���@��7@�7L@���@�Ĝ@��9@��D@�r�@�9X@��m@�ƨ@�\)@���@��@���@�@���@���@��7@��@�hs@�7L@���@���@�z�@�Z@�I�@��@���@���@�l�@�o@��R@�~�@�ff@�E�@��@��-@�`B@�&�@�V@��@���@��@��@�j@�z�@�bN@�Z@�I�@\)@~�y@~��@}�h@}O�@|�D@|(�@{�m@{t�@{"�@z�@z��@z�\@zn�@y�@x�`@x1'@w|�@w\)@w
=@v��@u�T@u�h@uV@t�j@tj@s�
@s��@st�@r��@rn�@r�@q�7@p�u@p �@pb@pb@pb@p  @o��@ol�@o+@n�+@m��@m?}@lz�@k�F@kt�@k"�@j��@i��@ihs@iG�@i7L@hĜ@h1'@g�P@g;d@f�y@f�+@f{@e@e�@d��@d�@d�@dj@c��@c�
@c�@b�H@b~�@a�^@`�`@`�u@`Q�@_�@_|�@_;d@^ȴ@^v�@^E�@]�@]p�@\�@\�@[�F@Z�H@Z�!@ZM�@Z�@Y��@Y�#@YG�@X��@X��@XbN@XA�@W�P@V�R@VV@V@U�-@U?}@T�/@T�j@T�@Tz�@T�@St�@R�@R^5@RJ@Q�^@Q�@P�9@PA�@O�;@O\)@N�R@Nff@N5?@N{@M�@M�T@M�@L��@L9X@L1@K��@K�
@K��@K�@KS�@K33@Ko@J��@J��@J~�@JM�@I��@I��@IX@I�@H��@H��@HĜ@H�@H�@HbN@H1'@H  @G�;@G�@G\)@F��@F��@F$�@E�@EO�@EV@D�/@D��@Dz�@DZ@D9X@D1@C�
@C��@CdZ@C"�@B�@B��@Bn�@A�@A7L@A%@@��@@��@@��@@Ĝ@@�9@@�@@Q�@@A�@?�P@?�@>�@>�R@>V@=�h@=p�@=`B@=?}@=V@<��@<��@<�/@<9X@;�m@;�
@;ƨ@;S�@;@:�H@:~�@:M�@:-@:J@9�#@9��@9�7@9G�@9&�@8�`@8�9@8Q�@7�P@7�@7
=@6v�@65?@5�@5��@5@5�-@5p�@5V@4��@4z�@4j@4I�@41@3�@3"�@2��@2�@1��@1x�@1G�@0��@0Q�@01'@/�@/;d@/;d@/+@.��@.�R@.E�@-��@-�@,��@,��@,z�@,Z@+��@+t�@+C�@+33@+33@+o@*��@*n�@)��@)hs@)�@(��@(�@(Q�@'�;@'l�@';d@'
=@&�@&ȴ@&��@&V@%��@%O�@%/@%V@$�@$�j@$9X@#��@#�m@#�F@#��@#S�@"�@"��@"��@"-@"J@!��@!�#@!x�@!&�@ �`@ ��@ r�@ bN@ Q�@ A�@ 1'@  �@   @�@�P@|�@K�@
=@�@�R@�R@��@��@�+@V@5?@{@@O�@/@�@�@/@/@�@�@��@��@�@j@I�@��@�
@ƨ@�F@��@dZ@dZ@S�@"�@�H@n�@=q@��@�#@��@hs@7L@%@��@�9@��@�@Q�@A�@  @��@�@�P@K�@+@+@+@
=@ȴ@��@ff@V@V@E�@{@�@@��@��@��@�h@�h@�h@�@`B@V@�/@��@��@�@�D@Z@9X@�
@��@��@�@t�@dZ@C�@33@@��@��@n�@M�@=q@�@�#@�^@�7@hs@�@��@�9@�@bN@  @�@|�@;d@
=@ȴ@ff@5?@5?@5?@5?@5?@@�T@�-@�h@�@p�@?}@V@��@�@��@�m@�m@�
@�F@t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�!A�9A�RA�A��A�A��A��A��A��A��A���A��A��A���A���A���A��\A��A��A�z�A�l�A�\)A�(�A���A��`A܋DA�K�A�t�A�/A��A���A�ȴA�M�AՅA��AԁA�VA�5?A��AЗ�A�jAω7A�O�A��
A�dZA�S�A�(�A���A�7LAŝ�A�ZA���A��TA���A�VA�=qA��A�A��yA���A�v�A�A�A��A��PA�A��A���A��A�(�A�\)A��RA�p�A�ȴA�A��mA�l�A�{A��7A��A��hA�/A�A�r�A�?}A�`BA�XA� �A��FA�\)A�XA�ȴA�{A��A��A�  A�&�A�$�A��9A��A��^A��mA�7LA��A�A��uA�dZA��A{O�AyhsAx�uAw�7Au%Aq+Al��Ai��AeAd$�Ab��Ab=qA`Q�A]O�A\�A\ �A[�^A[%AZ~�AZAV�/ATbAR�jAQ|�AO��AN=qAM+AL5?AK%AI��AHffAG�AFAEC�AD��AC�AB��AA�A?�7A>�A=�#A=33A<~�A;S�A:ĜA9�mA9"�A8(�A6(�A3t�A2ĜA2 �A1C�A/t�A.��A.5?A-oA,�A)�TA(�uA(=qA'��A'��A'"�A$ffA"�!A!|�A �jA v�A�^AJAO�A�jA��AdZA�DA�A��A�AJAoA��AƨAbNA33A��A�7A"�A5?A��A
ffA	�hA	+A9XAl�A��At�A�!A�PA"�AA�#A ��A �!A n�@�\)@��@���@� �@��P@�^5@���@��;@��@�ff@�O�@��@���@�ff@��@�E�@�@�+@�^5@�5?@��#@蛦@�bN@�K�@�{@�w@�h@��u@��
@ߍP@��@�@܋D@�ƨ@��@�ff@�M�@�$�@�Ĝ@׮@��H@�5?@���@�X@�bN@�;d@�~�@�J@�Ĝ@θR@�=q@��@��@���@͉7@�r�@�C�@���@ȴ9@�1@�dZ@�C�@���@�v�@�M�@���@�%@ċD@��m@�"�@��T@��/@�Z@�n�@�@���@�9X@��
@�C�@��@��@�33@��/@�r�@��F@��H@�$�@�`B@���@�bN@�ƨ@��H@�v�@�=q@�hs@��@��@�"�@��!@�t�@��R@�5?@���@���@�Ĝ@�\)@�-@��@��@���@��@��@�-@���@���@�5?@���@���@���@��7@�J@��\@�M�@�G�@��F@�E�@�5?@�5?@���@�t�@���@��u@�/@��@��;@�"�@�
=@���@���@���@��\@�$�@�V@���@�r�@���@��@��H@��@�C�@�S�@���@���@�{@�$�@���@�V@�E�@�$�@��@���@�G�@��9@� �@�ƨ@�|�@�dZ@�l�@�|�@�dZ@�S�@�S�@�K�@�33@��@���@�ȴ@��7@�Ĝ@��D@�A�@���@���@���@���@���@�dZ@�"�@�M�@���@��@���@��@�-@��!@���@���@��\@�~�@�{@���@���@�X@�7L@�V@��j@�I�@�9X@� �@�  @��@���@�C�@��R@�~�@�n�@�E�@�$�@�@���@�?}@��@�%@���@���@���@�@���@��+@�~�@�V@�E�@�-@��T@�@��-@���@�`B@�?}@�/@��@�bN@�1'@��@��
@��w@�|�@�33@��y@��@���@�n�@�V@�@��@��^@���@�hs@�%@�Ĝ@���@��@�Q�@�9X@�b@�t�@�ȴ@�^5@���@���@�7L@��@���@��/@���@��@��P@�|�@�33@��y@�V@���@��@���@���@��7@�7L@���@�Ĝ@��9@��D@�r�@�9X@��m@�ƨ@�\)@���@��@���@�@���@���@��7@��@�hs@�7L@���@���@�z�@�Z@�I�@��@���@���@�l�@�o@��R@�~�@�ff@�E�@��@��-@�`B@�&�@�V@��@���@��@��@�j@�z�@�bN@�Z@�I�@\)@~�y@~��@}�h@}O�@|�D@|(�@{�m@{t�@{"�@z�@z��@z�\@zn�@y�@x�`@x1'@w|�@w\)@w
=@v��@u�T@u�h@uV@t�j@tj@s�
@s��@st�@r��@rn�@r�@q�7@p�u@p �@pb@pb@pb@p  @o��@ol�@o+@n�+@m��@m?}@lz�@k�F@kt�@k"�@j��@i��@ihs@iG�@i7L@hĜ@h1'@g�P@g;d@f�y@f�+@f{@e@e�@d��@d�@d�@dj@c��@c�
@c�@b�H@b~�@a�^@`�`@`�u@`Q�@_�@_|�@_;d@^ȴ@^v�@^E�@]�@]p�@\�@\�@[�F@Z�H@Z�!@ZM�@Z�@Y��@Y�#@YG�@X��@X��@XbN@XA�@W�P@V�R@VV@V@U�-@U?}@T�/@T�j@T�@Tz�@T�@St�@R�@R^5@RJ@Q�^@Q�@P�9@PA�@O�;@O\)@N�R@Nff@N5?@N{@M�@M�T@M�@L��@L9X@L1@K��@K�
@K��@K�@KS�@K33@Ko@J��@J��@J~�@JM�@I��@I��@IX@I�@H��@H��@HĜ@H�@H�@HbN@H1'@H  @G�;@G�@G\)@F��@F��@F$�@E�@EO�@EV@D�/@D��@Dz�@DZ@D9X@D1@C�
@C��@CdZ@C"�@B�@B��@Bn�@A�@A7L@A%@@��@@��@@��@@Ĝ@@�9@@�@@Q�@@A�@?�P@?�@>�@>�R@>V@=�h@=p�@=`B@=?}@=V@<��@<��@<�/@<9X@;�m@;�
@;ƨ@;S�@;@:�H@:~�@:M�@:-@:J@9�#@9��@9�7@9G�@9&�@8�`@8�9@8Q�@7�P@7�@7
=@6v�@65?@5�@5��@5@5�-@5p�@5V@4��@4z�@4j@4I�@41@3�@3"�@2��@2�@1��@1x�@1G�@0��@0Q�@01'@/�@/;d@/;d@/+@.��@.�R@.E�@-��@-�@,��@,��@,z�@,Z@+��@+t�@+C�@+33@+33@+o@*��@*n�@)��@)hs@)�@(��@(�@(Q�@'�;@'l�@';d@'
=@&�@&ȴ@&��@&V@%��@%O�@%/@%V@$�@$�j@$9X@#��@#�m@#�F@#��@#S�@"�@"��@"��@"-@"J@!��@!�#@!x�@!&�@ �`@ ��@ r�@ bN@ Q�@ A�@ 1'@  �@   @�@�P@|�@K�@
=@�@�R@�R@��@��@�+@V@5?@{@@O�@/@�@�@/@/@�@�@��@��@�@j@I�@��@�
@ƨ@�F@��@dZ@dZ@S�@"�@�H@n�@=q@��@�#@��@hs@7L@%@��@�9@��@�@Q�@A�@  @��@�@�P@K�@+@+@+@
=@ȴ@��@ff@V@V@E�@{@�@@��@��@��@�h@�h@�h@�@`B@V@�/@��@��@�@�D@Z@9X@�
@��@��@�@t�@dZ@C�@33@@��@��@n�@M�@=q@�@�#@�^@�7@hs@�@��@�9@�@bN@  @�@|�@;d@
=@ȴ@ff@5?@5?@5?@5?@5?@@�T@�-@�h@�@p�@?}@V@��@�@��@�m@�m@�
@�F@t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ȴB
ȴB
ǮB
ǮB
ƨB
ƨB
ŢB
ÖB
��B
�RB
��B
�B
|�B
��B
�)B
�HB
�B
��B
�!B
�9B
�XB
ŢB
�)B
�B
�BPB$�Bu�B�wBɺB�ZB�wB��B�BB,B.B&�B(�BW
BZBH�B1'B�1B�jBŢB�qB�ZB��B�NB��BB��B��B�B�;B��Bl�B@�B�BB�BƨB��B�hB�%Bp�Bv�B�B� Bz�B� B}�B�uB��B��B�+Bo�BT�BF�B5?B#�B+B
�5B
��B
��B
�uB
�JB
�VB
�hB
�\B
�PB
�=B
�B
jB
R�B
J�B
<jB
&�B
	7B	�B	�ZB	�B	��B	��B	�qB	�?B	��B	��B	��B	��B	��B	�{B	�bB	|�B	iyB	e`B	^5B	W
B	O�B	H�B	D�B	=qB	6FB	.B	'�B	!�B	�B	�B	�B	hB	
=B	B	B��B��B��B�B�B�B�sB�ZB�;B��B��B��B��BĜB��B�}B�dB�^B�3B�B�B�B�B��B��B��B��B��B��B��B��B�uB�hB�VB�JB�7B�%B�B}�B|�Bz�By�Bx�Bv�Bt�Bs�Br�Bq�Bo�Bn�Bl�BiyBhsBffBffBe`Be`Be`BgmBffBdZBffBe`BdZBcTBdZBdZBe`Be`BgmBiyBjBhsBk�Bn�Bn�Bp�Bp�Bp�Bp�Bp�Bq�Br�Bq�Bq�Bq�Bs�Bt�Bu�Bw�B~�B�B�+B�=B�DB�PB�hB��B��B��B��B��B��B��B�B�!B�?B�LB�XB�qBÖBǮB��B��B��B��B��B��B��B��B�B�;B�yB�yB�B�B��B��B��B��B��B��B��B	  B	B	B	B	B��B	B	uB	�B	�B	#�B	&�B	+B	5?B	49B	49B	5?B	49B	9XB	<jB	?}B	E�B	M�B	R�B	S�B	T�B	T�B	VB	VB	XB	ZB	e`B	k�B	l�B	n�B	n�B	n�B	k�B	k�B	q�B	w�B	|�B	� B	�B	�1B	�1B	�JB	�\B	�DB	�=B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�FB	�LB	�RB	�FB	�FB	�FB	�?B	�?B	�?B	�LB	�jB	�dB	�dB	�XB	�RB	�qB	��B	ĜB	��B	��B	��B	��B	��B	�/B	�;B	�;B	�;B	�HB	�NB	�TB	�`B	�`B	�ZB	�TB	�ZB	�ZB	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�fB	�TB	�ZB	�ZB	�TB	�ZB	�`B	�fB	�fB	�`B	�ZB	�ZB	�TB	�TB	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
1B
	7B
	7B
1B
%B
	7B

=B
DB
DB

=B

=B

=B

=B
DB

=B
	7B
+B
B
+B
%B
B
B
B
B
%B
+B
+B
	7B

=B

=B

=B

=B

=B
DB
JB
PB
PB
VB
VB
VB
VB
\B
\B
bB
\B
\B
\B
\B
\B
bB
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
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
"�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
,B
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
.B
.B
/B
/B
/B
/B
0!B
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
6FB
6FB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
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
F�B
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
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
I�B
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
P�B
Q�B
R�B
R�B
R�B
S�B
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
YB
YB
YB
YB
YB
YB
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
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
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
`BB
aHB
aHB
bNB
bNB
bNB
bNB
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
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
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
ffB
ffB
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
hsB
hsB
hsB
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
jB
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
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
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
r�B
r�B
r�B
s�B
s�B
s�B
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
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�gB
��B
��B
��B
�SB
}B
��B
��B
�B
�1B
��B
�-B
�tB
�B
�1B
�jB
ܬB
�BVB'RBx8B��B�<B�QB�BˬB�BSB,�B/�B)*B+kBYKB]�BJrB1AB��B�wB��B�B�>B�B�ZB��BGB�B��B��B�B��Bq�BE�B+B�B&2B��B��B�{B�	BshBw�B��B��B}<B�B~�B��B�B��B��Bs3BV�BH�B8RB(�B�B
��B
ĜB
��B
��B
��B
�(B
�B
�.B
�VB
�JB
�7B
l�B
T�B
L�B
@4B
+�B
VB	�B	�B	�+B	ΊB	��B	�B	�8B	��B	��B	��B	��B	��B	�9B	�B	�B	kkB	g8B	`\B	Y1B	QhB	JXB	FYB	?cB	8B	/�B	)yB	"�B	�B	B	yB	�B	JB	YB	uB�(B�B�LB�B�B��B�eB�8B�4B�B�B�vB��B��B��B�;B�VB�"B��B��B��B��B��B�]B�$B�vB��B��B��B��B��B��B��B��B�<B��B�7B�uBB~]B|B{Bz�Bx�Bv�BtTBs�Br�Bp�BpUBm�Bj�Bi�Bg�Bg�BgBf�Bf�Bh$Bg8BfBg�BfBe,Bd�BezBe,BfLBfBhsBjBkkBiBlqBo�Bo�Bq�Bq�BrGBraBrGBraBshBrBraBr�Bt9Bu�BwBy�B��B��B��B��B��B��B�TB�SB�EB�)B�B�OB��B��B��B��B��B�B�DB�wB�gBȀB�B�:B�bB�4B�NB�hBуB� B�7B�B��B�0B�5B�B�+B�^B�^B��B��B��B��B	 �B	9B	B	�B	-B�HB	aB	uB	B	OB	$@B	'RB	+kB	6�B	4�B	5B	6B	5B	:B	=B	@4B	F?B	N�B	SuB	T{B	U�B	U�B	V�B	V�B	XyB	Z7B	e�B	lB	mB	oB	o�B	o�B	lWB	k�B	q�B	xB	}B	�4B	�gB	��B	�fB	��B	�}B	��B	��B	�&B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�'B	��B	�B	��B	��B	��B	�zB	��B	��B	��B	�8B	��B	�B	�B	��B	��B	��B	��B	��B	�DB	�JB	�BB	��B	�B	ݘB	ߊB	ߊB	ߊB	��B	��B	��B	��B	��B	�B	�B	�tB	�B	�B	�B	�B	�B	�B	��B	��B	��B	�eB	�B	�B	��B	��B	�B	�tB	�B	�B	��B	��B	�B	�B	�B	�B	�B	�fB	�mB	��B	��B	��B	��B	� B	�B	��B	�B	�B	�B	�%B	�?B	��B	��B	�B	�B	�?B	�FB	�RB	�DB	�0B	�0B	�0B	�0B	�jB	�<B	�BB	�VB	��B	�qB	�JB	�^B	�JB	�"B	�<B	�<B	�(B	�(B	�]B
 OB
;B
;B
uB
aB
aB
{B
�B
gB
mB
YB
tB
�B
tB
zB
zB
�B
�B
	lB
	�B
�B
?B
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
�B
�B
�B
�B
mB
mB
MB
mB
�B
�B
zB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
�B
�B
�B
�B
B
B
�B
	B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
OB
B
B
!B
!B
;B
 'B
!B
!-B
"B
"B
# B
# B
#B
#:B
#nB
#TB
"4B
#B
#:B
# B
#TB
$&B
$@B
%,B
%,B
&LB
&2B
&2B
&LB
'8B
'RB
'mB
(sB
)DB
)*B
*0B
)*B
)DB
)DB
*KB
*eB
*B
+kB
,qB
,�B
-wB
-CB
-]B
-wB
-wB
.cB
.IB
.cB
.}B
.cB
.cB
/iB
/OB
/�B
/�B
0�B
0oB
0oB
1[B
1[B
1vB
1�B
2|B
2�B
2�B
2|B
3�B
3�B
4�B
4nB
4�B
4�B
4�B
4�B
5�B
5tB
5�B
5�B
6�B
6�B
7�B
7�B
8�B
8�B
8�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
:�B
;�B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
?�B
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
EB
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
F�B
F�B
GB
F�B
GB
GB
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
IB
IB
IB
IB
IB
IB
J#B
J	B
I�B
J�B
I�B
J�B
KB
KB
KB
KB
K)B
LB
LB
K�B
L0B
L0B
MB
MB
MB
MB
MB
MB
MB
M6B
N"B
N"B
N"B
N<B
OB
O(B
O(B
P.B
P.B
PB
P.B
P.B
P.B
Q4B
QB
Q4B
QB
Q4B
RTB
S[B
S&B
S[B
TFB
TFB
TFB
T,B
T,B
TFB
TaB
UMB
UMB
UMB
U2B
UMB
UMB
VmB
VSB
VmB
WYB
WYB
WYB
X_B
X_B
YeB
YeB
YeB
YKB
YeB
YKB
ZkB
Z�B
[�B
[�B
[qB
[qB
[qB
\]B
\�B
\�B
\xB
]IB
]dB
]~B
]~B
]�B
^�B
^�B
^�B
_pB
_�B
_�B
_�B
_�B
`vB
`vB
`vB
`vB
`�B
`�B
a�B
a�B
b�B
b�B
b�B
b�B
a�B
b�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
d�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
ezB
e�B
ezB
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
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
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
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
r�B
r�B
r�B
tB
tB
tB
uB
t�B
uB
uB
vB
vB
vB
vB
vB
u�B
u�B
u�B
u�B
vB
vB
wB
v�B
v�B
v�B
wB
wB
xB
x8B
x�B
y	B
x�B
x�B
y$B
y$B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.34(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608110036582016081100365820160811003658201806221300272018062213002720180622130027201804050659192018040506591920180405065919  JA  ARFMdecpA19c                                                                20160807093504  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160807003517  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160807003517  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160807003517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160807003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160807003518  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160807003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160807003518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160807003518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160807003519                      G�O�G�O�G�O�                JA  ARUP                                                                        20160807012039                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160807153620  CV  JULD            G�O�G�O�F�	�                JM  ARCAJMQC2.0                                                                 20160810153658  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160810153658  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404215919  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040027  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201516                      G�O�G�O�G�O�                