CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-03-10T15:02:01Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        |  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  `(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �      TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȱ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ̐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220310150201  20220310150201  4903319 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               FA   AO  8280                            2B  A   NAVIS_A                         1159                            170425                          863 @ٿ�ۥ�1   @ٿ��%��@8�V��d�-V1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         FA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&y�D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`fD`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�<�Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @}p�@��@��A�\A:�\AZ�\Az�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��HC��{D j=D �=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=D	j=D	�=D
j=D
�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=D j=D �=D!j=D!�=D"j=D"�=D#j=D#�=D$j=D$�=D%j=D%�=D&c�D&�=D'j=D'�=D(j=D(�=D)j=D)�=D*j=D*�=D+j=D+�=D,j=D,�=D-j=D-�=D.j=D.�=D/p�D/�D0j=D0�=D1j=D1�=D2j=D2�=D3j=D3�=D4j=D4�=D5j=D5�=D6j=D6�=D7j=D7�=D8j=D8�=D9j=D9�=D:j=D:�=D;j=D;�=D<j=D<�=D=j=D=�=D>j=D>�=D?j=D?�=D@j=D@�=DAj=DA�=DBj=DB�=DCj=DC�=DDj=DD�=DEj=DE�=DFj=DF�=DGj=DG�=DHj=DH�=DIj=DI�=DJj=DJ�=DKj=DK�=DLj=DL�=DMj=DM�=DNj=DN�=DOj=DO�=DPj=DP�=DQj=DQ�=DRj=DR�=DSj=DS�=DTj=DT�=DUj=DU�=DVj=DV�=DWj=DW�=DXj=DX�=DYj=DY�=DZj=DZ�=D[j=D[�=D\j=D\�=D]j=D]�=D^j=D^�=D_j=D_�D`j=D`�=Daj=Da�=Dbj=Db�=Dcj=Dc�=Ddj=Dd�=Dej=De�=Dfj=Df�=Dgj=Dg�=Dhj=Dh�=Dij=Di�=Djj=Dj�=Dkj=Dk�=Dlj=Dl�=Dmj=Dm�=Dnj=Dn�=Doj=Do�=Dpj=Dp�=Dqj=Dq�=Drj=Dr�=Dsj=Ds�=Dtj=Dt�=Duj=Du�=Dvj=Dv�=Dwj=Dw�=Dxj=Dx�=Dyj=Dy�=Dzj=Dz�=D{j=D{�=D|j=D|�=D}j=D}�=D~j=D~�=Dj=D�=D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�q�D��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uDµD��D�5D�uDõD��D�5D�uDĵD��D�1�D�uDŵD��D�5D�uDƵD��D�5D�uDǵD��D�5D�uDȵD��D�5D�uDɵD��D�5D�uDʵD��D�5D�uD˵D��D�5D�uD̵D��D�5D�uD͵D��D�5D�uDεD��D�5D�uDϵD��D�5D�uDеD��D�5D�uDѵD��D�5D�uDҵD��D�5D�uDӵD��D�5D�uDԵD��D�5D�uDյD��D�5D�uDֵD��D�5D�uD׵D��D�5D�uDصD��D�5D�uDٵD��D�5D�uDڵD��D�5D�uD۵D��D�5D�uDܵD��D�5D�uDݵD��D�5D�uD޵D��D�5D�uDߵD��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�xRD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD��D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��RD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�33A��A�
=A�A�  A�  A���A���A��A��A��A��A��A��mA��HA��
A�ȴA��A���A���A��DA��A�l�A�`BA�oA��A��A��A�|�A�VA�5?A��A���A�7LA��A��`A���A�=qA���A���A���A���A�t�A�7LA��A��A��A���A��`A��A�ȴA��^A��7A�$�A�|�A���A��
A�$�A��jA�n�A��PA�K�A�l�A���A��PA�&�A��PA��jA���A���A��FA�$�A���A�-A�ĜA� �A���A�{A�ȴA�ffA�E�A���A�l�A�9XA���A���A���A�dZA���A�oA��7A�G�A�1A�=qA�n�A��RA�^5A��hA�bA�^5A��7A�v�A�hsA��A�jA�&�A���A���A�dZA���A���A��\A��7A�|�A�S�A��wA~~�A|�/A{�AzbNAx�HAu�AsS�Ar�`Ao�
Aj1'Ah  Af��Af �AeK�Ab�A^��A\  AZ��AY;dAUƨAR�AQ`BAQ
=APv�AOƨAN�9AK�
AJ��AI�TAH�DAG��AG33AEt�AC��AB�A@�A?�#A>ĜA=C�A<��A;�^A97LA8A�A7t�A6�!A5�-A4��A3�
A3`BA3VA2��A1ƨA0�uA/�hA.�/A.1'A-�A,�yA,��A,(�A*�A)`BA(ȴA( �A&jA&A%�A$1A#��A"��A!A ��A ��A �DA VA�mAbNAdZA(�AdZAA��A�AhsA��A�jAM�AK�A��A1'AK�A�\A�A��A�hA&�A�DA��A��AbA33A�HA �A��A|�A
A�HA�A��A�A+A�/AQ�A�A��AA-A�wAS�A �j@��m@��@���@�ff@�ff@��@�33@�`B@�C�@�/@�ȴ@�^@噚@��@�1'@��
@��@�%@�Q�@ߍP@��#@ۅ@��@�r�@��;@��H@��@�1'@�-@ѡ�@�Ĝ@�"�@���@��;@�;d@�33@�33@�
=@ɺ^@���@�M�@�?}@�bN@å�@¸R@�X@��@��@��@�bN@�Q�@�  @���@�S�@��@�X@�?}@�/@��u@�Q�@���@�C�@�o@�v�@��@��T@�/@�l�@�J@�V@�I�@���@��F@��P@�dZ@��H@�^5@�@�/@���@� �@��w@�\)@��H@�E�@���@��@�Ĝ@�I�@��;@��@�dZ@�33@��@���@��@���@���@���@�t�@��!@�V@�=q@�$�@���@��@��u@��;@�t�@�@���@�E�@�J@���@��@��j@�r�@�1@���@���@���@�z�@�bN@�z�@�9X@�33@�M�@���@�Q�@�9X@�1'@�9X@��@��w@�dZ@��@���@�^5@�J@��-@�?}@��`@��`@��j@�bN@��@��;@�ƨ@��w@���@�33@�@��\@�-@�J@��@���@�X@��@���@�I�@��
@��@�+@��y@���@���@�p�@�7L@���@���@��@�j@�A�@�9X@�b@�"�@�V@�5?@�{@��T@���@�@�x�@�V@���@�Z@�1'@��@���@��!@��+@�E�@�5?@�-@��@�@��T@��^@�x�@�O�@�/@�&�@��@��@�V@��`@���@��9@��D@�z�@�Q�@� �@���@�ƨ@���@�|�@��@���@�~�@�5?@��T@��h@�hs@�%@���@���@�r�@�b@�b@� �@�;@+@~�R@~��@~v�@~{@}��@|Z@{S�@z�H@z��@z�!@z��@z�!@z��@z�\@z^5@z=q@y��@y%@x�u@xQ�@x  @w�w@w
=@vV@u�@u��@u?}@t�@t�j@t�D@tj@tj@tZ@t(�@t1@s�
@st�@s33@s@rn�@r=q@r�@q��@qhs@p��@p�9@p��@p  @n�y@nV@n{@m@m�@m`B@mp�@mp�@mp�@mO�@m?}@mV@l�/@lz�@l1@k��@kC�@j�@j�@i&�@h �@g�@g�w@g\)@g�@f��@f�R@f��@f�+@fV@f5?@e�@ep�@e�@d�D@d1@ct�@c@b��@b��@b�!@b~�@b-@bJ@a�#@ax�@a7L@`�@` �@`  @_�@]?}@\�D@\z�@[�F@[@Z~�@Z�!@Z��@Y�@YG�@Y&�@X�9@Xr�@X �@W\)@W;d@W+@W�@V�y@V�R@V��@Vv�@VE�@V5?@V$�@V{@U�@U��@U@U�-@U�-@U��@U?}@T�@T�/@T��@T��@T��@T�D@TI�@S��@S�F@S��@SdZ@SS�@S33@So@R~�@RJ@Qhs@P��@P�@O��@O
=@Nv�@Nff@NV@NE�@N5?@M�@M�h@M�@L��@Lz�@K�F@J��@J��@JJ@Ihs@HĜ@HbN@Hb@G�@F�y@FV@F@F@E�T@Ep�@EV@C�m@B�\@B-@B=q@BJ@A��@A�7@Ahs@Ahs@@�`@@bN@@bN@@A�@@Q�@@ �@@b@@  @?�;@?��@?�P@?l�@?K�@?;d@?K�@?K�@>�y@>��@>��@>$�@=�@=�h@=O�@=/@<��@<9X@<�@;ƨ@;"�@:�H@:n�@:M�@:J@9�7@9&�@9�@8��@8�9@8��@8�@8b@7�;@7�;@7�;@7�w@7�P@7;d@7�@6ȴ@6$�@6$�@5@5`B@4�@4�j@4z�@4(�@41@3��@3�F@3�@3S�@333@3o@2�@2�@2�H@2��@2M�@2�@1��@1hs@1G�@17L@0�u@0  @/�@/��@/+@.�R@.ff@.V@-�@-/@,��@,��@,�j@,Z@+�
@+C�@+"�@+@*�H@*��@*~�@*^5@*M�@*=q@*J@)��@)�@(Ĝ@( �@'��@'K�@&�y@&ȴ@&�R@&��@&�+@&5?@%��@%@%��@%�h@%�@%V@$��@$(�@#�F@#��@#dZ@#"�@"��@"��@"M�@!�#@!�^@!hs@ ��@ Ĝ@ Q�@ b@��@�w@�@�@+@
=@
=@�y@E�@{@�T@@��@�h@�@�@��@j@�m@t�@"�@o@@�@��@M�@��@G�@�@��@��@�`@��@�9@r�@ �@  @�;@�@
=@�R@v�@$�@�h@/@�@��@��@��@��@��@�@�/@�j@�j@�@�@�@�@��@�D@1@��@��@o@��@�\@~�@n�@n�@^5@M�@-@�@�@��@��@x�@hs@X@X@X@G�@�@�@�`@��@�@Q�@A�@ �@�@�;@��@�P@;d@ȴ@E�@E�@5?@{@�T@�T@��@@@�@/@V@�@��@�j@�@��@�D@j@9X@1@�
@�@S�@
�H@
n�@
M�@
-@
J@	�@	��@	X@	%@��@��@�u@bN@Q�@A�@1'@  @��@��@�w@�P@l�@�@��@��@��@v�@E�@$�@@p�@?}@/@/@�@��@�@j@I�@(�@��@�m@�
@ƨ@dZ@C�@33@"�@@��@~�@^5@^5@^5@M�@=q@�#@�^@�^@��@��@��@��@x�@G�@7L@7L@&�@%@%@ ��@ Ĝ@ �u@ r�@ Q�@ A�@ 1'@ b@  �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�33A��A�
=A�A�  A�  A���A���A��A��A��A��A��A��mA��HA��
A�ȴA��A���A���A��DA��A�l�A�`BA�oA��A��A��A�|�A�VA�5?A��A���A�7LA��A��`A���A�=qA���A���A���A���A�t�A�7LA��A��A��A���A��`A��A�ȴA��^A��7A�$�A�|�A���A��
A�$�A��jA�n�A��PA�K�A�l�A���A��PA�&�A��PA��jA���A���A��FA�$�A���A�-A�ĜA� �A���A�{A�ȴA�ffA�E�A���A�l�A�9XA���A���A���A�dZA���A�oA��7A�G�A�1A�=qA�n�A��RA�^5A��hA�bA�^5A��7A�v�A�hsA��A�jA�&�A���A���A�dZA���A���A��\A��7A�|�A�S�A��wA~~�A|�/A{�AzbNAx�HAu�AsS�Ar�`Ao�
Aj1'Ah  Af��Af �AeK�Ab�A^��A\  AZ��AY;dAUƨAR�AQ`BAQ
=APv�AOƨAN�9AK�
AJ��AI�TAH�DAG��AG33AEt�AC��AB�A@�A?�#A>ĜA=C�A<��A;�^A97LA8A�A7t�A6�!A5�-A4��A3�
A3`BA3VA2��A1ƨA0�uA/�hA.�/A.1'A-�A,�yA,��A,(�A*�A)`BA(ȴA( �A&jA&A%�A$1A#��A"��A!A ��A ��A �DA VA�mAbNAdZA(�AdZAA��A�AhsA��A�jAM�AK�A��A1'AK�A�\A�A��A�hA&�A�DA��A��AbA33A�HA �A��A|�A
A�HA�A��A�A+A�/AQ�A�A��AA-A�wAS�A �j@��m@��@���@�ff@�ff@��@�33@�`B@�C�@�/@�ȴ@�^@噚@��@�1'@��
@��@�%@�Q�@ߍP@��#@ۅ@��@�r�@��;@��H@��@�1'@�-@ѡ�@�Ĝ@�"�@���@��;@�;d@�33@�33@�
=@ɺ^@���@�M�@�?}@�bN@å�@¸R@�X@��@��@��@�bN@�Q�@�  @���@�S�@��@�X@�?}@�/@��u@�Q�@���@�C�@�o@�v�@��@��T@�/@�l�@�J@�V@�I�@���@��F@��P@�dZ@��H@�^5@�@�/@���@� �@��w@�\)@��H@�E�@���@��@�Ĝ@�I�@��;@��@�dZ@�33@��@���@��@���@���@���@�t�@��!@�V@�=q@�$�@���@��@��u@��;@�t�@�@���@�E�@�J@���@��@��j@�r�@�1@���@���@���@�z�@�bN@�z�@�9X@�33@�M�@���@�Q�@�9X@�1'@�9X@��@��w@�dZ@��@���@�^5@�J@��-@�?}@��`@��`@��j@�bN@��@��;@�ƨ@��w@���@�33@�@��\@�-@�J@��@���@�X@��@���@�I�@��
@��@�+@��y@���@���@�p�@�7L@���@���@��@�j@�A�@�9X@�b@�"�@�V@�5?@�{@��T@���@�@�x�@�V@���@�Z@�1'@��@���@��!@��+@�E�@�5?@�-@��@�@��T@��^@�x�@�O�@�/@�&�@��@��@�V@��`@���@��9@��D@�z�@�Q�@� �@���@�ƨ@���@�|�@��@���@�~�@�5?@��T@��h@�hs@�%@���@���@�r�@�b@�b@� �@�;@+@~�R@~��@~v�@~{@}��@|Z@{S�@z�H@z��@z�!@z��@z�!@z��@z�\@z^5@z=q@y��@y%@x�u@xQ�@x  @w�w@w
=@vV@u�@u��@u?}@t�@t�j@t�D@tj@tj@tZ@t(�@t1@s�
@st�@s33@s@rn�@r=q@r�@q��@qhs@p��@p�9@p��@p  @n�y@nV@n{@m@m�@m`B@mp�@mp�@mp�@mO�@m?}@mV@l�/@lz�@l1@k��@kC�@j�@j�@i&�@h �@g�@g�w@g\)@g�@f��@f�R@f��@f�+@fV@f5?@e�@ep�@e�@d�D@d1@ct�@c@b��@b��@b�!@b~�@b-@bJ@a�#@ax�@a7L@`�@` �@`  @_�@]?}@\�D@\z�@[�F@[@Z~�@Z�!@Z��@Y�@YG�@Y&�@X�9@Xr�@X �@W\)@W;d@W+@W�@V�y@V�R@V��@Vv�@VE�@V5?@V$�@V{@U�@U��@U@U�-@U�-@U��@U?}@T�@T�/@T��@T��@T��@T�D@TI�@S��@S�F@S��@SdZ@SS�@S33@So@R~�@RJ@Qhs@P��@P�@O��@O
=@Nv�@Nff@NV@NE�@N5?@M�@M�h@M�@L��@Lz�@K�F@J��@J��@JJ@Ihs@HĜ@HbN@Hb@G�@F�y@FV@F@F@E�T@Ep�@EV@C�m@B�\@B-@B=q@BJ@A��@A�7@Ahs@Ahs@@�`@@bN@@bN@@A�@@Q�@@ �@@b@@  @?�;@?��@?�P@?l�@?K�@?;d@?K�@?K�@>�y@>��@>��@>$�@=�@=�h@=O�@=/@<��@<9X@<�@;ƨ@;"�@:�H@:n�@:M�@:J@9�7@9&�@9�@8��@8�9@8��@8�@8b@7�;@7�;@7�;@7�w@7�P@7;d@7�@6ȴ@6$�@6$�@5@5`B@4�@4�j@4z�@4(�@41@3��@3�F@3�@3S�@333@3o@2�@2�@2�H@2��@2M�@2�@1��@1hs@1G�@17L@0�u@0  @/�@/��@/+@.�R@.ff@.V@-�@-/@,��@,��@,�j@,Z@+�
@+C�@+"�@+@*�H@*��@*~�@*^5@*M�@*=q@*J@)��@)�@(Ĝ@( �@'��@'K�@&�y@&ȴ@&�R@&��@&�+@&5?@%��@%@%��@%�h@%�@%V@$��@$(�@#�F@#��@#dZ@#"�@"��@"��@"M�@!�#@!�^@!hs@ ��@ Ĝ@ Q�@ b@��@�w@�@�@+@
=@
=@�y@E�@{@�T@@��@�h@�@�@��@j@�m@t�@"�@o@@�@��@M�@��@G�@�@��@��@�`@��@�9@r�@ �@  @�;@�@
=@�R@v�@$�@�h@/@�@��@��@��@��@��@�@�/@�j@�j@�@�@�@�@��@�D@1@��@��@o@��@�\@~�@n�@n�@^5@M�@-@�@�@��@��@x�@hs@X@X@X@G�@�@�@�`@��@�@Q�@A�@ �@�@�;@��@�P@;d@ȴ@E�@E�@5?@{@�T@�T@��@@@�@/@V@�@��@�j@�@��@�D@j@9X@1@�
@�@S�@
�H@
n�@
M�@
-@
J@	�@	��@	X@	%@��@��@�u@bN@Q�@A�@1'@  @��@��@�w@�P@l�@�@��@��@��@v�@E�@$�@@p�@?}@/@/@�@��@�@j@I�@(�@��@�m@�
@ƨ@dZ@C�@33@"�@@��@~�@^5@^5@^5@M�@=q@�#@�^@�^@��@��@��@��@x�@G�@7L@7L@&�@%@%@ ��@ Ĝ@ �u@ r�@ Q�@ A�@ 1'@ b@  �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bo�Bl�Bl�BjBiyBjBjBjBiyBiyBiyBiyBiyBiyBhsBiyBjBjBiyBiyBiyBiyBiyBiyBhsBjBo�Bz�B�B�\B�!B�qB��BBB��B��B�wB�qB�}B�}B�qB�qB�dB�jB�jB�jB�}B�}B�}B�wB�wB�qB�qB�RB�B��B��B��B��B��B��B�PB�7B�B�+Bv�Bn�BhsB_;BL�B=qB49B/B&�B�B�B��B�B�fB�BB��B�XB�'B�B�B��B��B��B��B�Bx�Bs�Bo�BdZBYBB�B.B{B
=BB
�B
�ZB
ɺB
�FB
�B
��B
��B
�{B
~�B
m�B
l�B
l�B
k�B
jB
gmB
_;B
M�B
A�B
9XB
2-B
'�B
�B
B
  B	�B	��B	�B	��B	��B	�hB	}�B	iyB	ZB	O�B	G�B	8RB	'�B	�B	�B	�B	{B	bB	B��B��B�B�B�B�`B�5B��B��B��BɺBÖB��B�qB�^B�9B�!B�B�B��B��B��B��B��B��B��B�\B�PB�DB�DB�7B�1B�+B�B�B}�B{�Bt�Bq�Bn�BiyBiyBffBdZBbNBaHB`BB_;B]/BZBW
BVBS�BR�BR�BQ�BQ�BP�BO�BN�BM�BK�BJ�BH�BF�BE�BD�BC�BB�BA�B?}B=qB;dB8RB7LB5?B33B0!B.B,B)�B(�B'�B'�B%�B%�B$�B#�B!�B�B�B�B�B�B�B�B�B�B�B�B�BuBuBuBoBhBoBhBhBhBoBhBbBhB{B{B�B�B�B�B�B�B�B�B�B�B"�B$�B%�B%�B%�B+B1'B8RB<jB?}BB�BE�BH�BH�BJ�BK�BM�BL�BM�BL�BM�BP�BR�BR�BS�BW
BW
BZB[#B[#B]/B]/B]/B_;BdZBgmBjBm�Bn�Bn�Bo�Bo�Bq�Bt�Bw�Bz�B}�B� B�B�B�B�B�B�+B�+B�7B�DB�PB�\B�\B�bB��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�FB�XB�dB�qB�wB��BĜBƨBǮBɺB��B��B��B��B�
B�B�)B�BB�NB�mB�yB�yB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	B	B	B		7B	JB	VB	bB	oB	{B	�B	�B	�B	 �B	"�B	#�B	&�B	(�B	)�B	+B	0!B	9XB	:^B	=qB	D�B	E�B	D�B	G�B	J�B	K�B	M�B	N�B	N�B	R�B	[#B	]/B	`BB	`BB	aHB	bNB	cTB	e`B	gmB	k�B	m�B	m�B	n�B	n�B	n�B	n�B	p�B	p�B	r�B	s�B	t�B	u�B	w�B	x�B	y�B	y�B	x�B	y�B	|�B	|�B	� B	�B	�%B	�%B	�1B	�7B	�DB	�PB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�LB	�RB	�XB	�jB	�qB	�wB	�}B	��B	��B	��B	��B	B	B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�;B	�HB	�ZB	�fB	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
%B
+B
1B
	7B
	7B
DB
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
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
{B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
$�B
$�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
'�B
'�B
'�B
'�B
'�B
(�B
+B
,B
,B
-B
-B
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
0!B
1'B
1'B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
49B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
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
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
=qB
<jB
<jB
=qB
>wB
>wB
?}B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
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
H�B
I�B
I�B
J�B
J�B
K�B
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
N�B
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
Q�B
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
VB
VB
VB
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
XB
XB
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
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
_;B
_;B
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
`BB
`BB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
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
e`B
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
m�B
m�B
m�B
m�B
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
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
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
w�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bo�Bl�Bl�BjBiyBjBjBjBiyBiyBiyBiyBiyBiyBhsBiyBjBjBiyBiyBiyBiyBiyBiyBhsBjBo�Bz�B�B�\B�!B�qB��BBB��B��B�wB�qB�}B�}B�qB�qB�dB�jB�jB�jB�}B�}B�}B�wB�wB�qB�qB�RB�B��B��B��B��B��B��B�PB�7B�B�+Bv�Bn�BhsB_;BL�B=qB49B/B&�B�B�B��B�B�fB�BB��B�XB�'B�B�B��B��B��B��B�Bx�Bs�Bo�BdZBYBB�B.B{B
=BB
�B
�ZB
ɺB
�FB
�B
��B
��B
�{B
~�B
m�B
l�B
l�B
k�B
jB
gmB
_;B
M�B
A�B
9XB
2-B
'�B
�B
B
  B	�B	��B	�B	��B	��B	�hB	}�B	iyB	ZB	O�B	G�B	8RB	'�B	�B	�B	�B	{B	bB	B��B��B�B�B�B�`B�5B��B��B��BɺBÖB��B�qB�^B�9B�!B�B�B��B��B��B��B��B��B��B�\B�PB�DB�DB�7B�1B�+B�B�B}�B{�Bt�Bq�Bn�BiyBiyBffBdZBbNBaHB`BB_;B]/BZBW
BVBS�BR�BR�BQ�BQ�BP�BO�BN�BM�BK�BJ�BH�BF�BE�BD�BC�BB�BA�B?}B=qB;dB8RB7LB5?B33B0!B.B,B)�B(�B'�B'�B%�B%�B$�B#�B!�B�B�B�B�B�B�B�B�B�B�B�B�BuBuBuBoBhBoBhBhBhBoBhBbBhB{B{B�B�B�B�B�B�B�B�B�B�B"�B$�B%�B%�B%�B+B1'B8RB<jB?}BB�BE�BH�BH�BJ�BK�BM�BL�BM�BL�BM�BP�BR�BR�BS�BW
BW
BZB[#B[#B]/B]/B]/B_;BdZBgmBjBm�Bn�Bn�Bo�Bo�Bq�Bt�Bw�Bz�B}�B� B�B�B�B�B�B�+B�+B�7B�DB�PB�\B�\B�bB��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�FB�XB�dB�qB�wB��BĜBƨBǮBɺB��B��B��B��B�
B�B�)B�BB�NB�mB�yB�yB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	B	B	B		7B	JB	VB	bB	oB	{B	�B	�B	�B	 �B	"�B	#�B	&�B	(�B	)�B	+B	0!B	9XB	:^B	=qB	D�B	E�B	D�B	G�B	J�B	K�B	M�B	N�B	N�B	R�B	[#B	]/B	`BB	`BB	aHB	bNB	cTB	e`B	gmB	k�B	m�B	m�B	n�B	n�B	n�B	n�B	p�B	p�B	r�B	s�B	t�B	u�B	w�B	x�B	y�B	y�B	x�B	y�B	|�B	|�B	� B	�B	�%B	�%B	�1B	�7B	�DB	�PB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�LB	�RB	�XB	�jB	�qB	�wB	�}B	��B	��B	��B	��B	B	B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�;B	�HB	�ZB	�fB	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
%B
+B
1B
	7B
	7B
DB
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
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
{B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
$�B
$�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
'�B
'�B
'�B
'�B
'�B
(�B
+B
,B
,B
-B
-B
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
0!B
1'B
1'B
2-B
33B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
49B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
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
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
=qB
<jB
<jB
=qB
>wB
>wB
?}B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
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
H�B
I�B
I�B
J�B
J�B
K�B
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
N�B
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
Q�B
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
VB
VB
VB
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
XB
XB
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
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
_;B
_;B
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
`BB
`BB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
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
e`B
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
m�B
m�B
m�B
m�B
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
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
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
w�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.34 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220310150201                              AO  ARCAADJP                                                                    20220310150201    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220310150201  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220310150201  QCF$                G�O�G�O�G�O�0               