CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-07-02T21:35:14Z creation;2017-07-02T21:35:17Z conversion to V3.1;2019-12-19T08:03:28Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20170702213514  20200116211516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_135                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @���Ӏ1   @����-�@3�@��4n�du�=�K1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @p��@��@��A�\A:�\AZ�\Az�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B�Q�B�Q�B�Q�B�Q�B�B�Q�B�Q�B�Q�B�Q�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D j=D �=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=D	j=D	�=D
j=D
�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=D j=D �=D!j=D!�=D"j=D"�=D#j=D#�=D$j=D$�=D%j=D%�=D&j=D&�=D'j=D'�=D(j=D(�=D)j=D)�=D*j=D*�=D+j=D+�=D,j=D,�=D-j=D-�=D.j=D.�=D/j=D/�=D0j=D0�=D1j=D1�=D2j=D2�=D3j=D3�=D4j=D4�=D5j=D5�=D6j=D6�=D7j=D7�=D8j=D8�=D9j=D9�=D:j=D:�=D;j=D;�=D<j=D<�=D=j=D=�=D>j=D>�=D?j=D?�=D@j=D@�=DAj=DA�=DBj=DB�=DCj=DC�=DDj=DD�=DEj=DE�=DFj=DF�=DGj=DG�=DHj=DH�=DIj=DI�=DJj=DJ�=DKj=DK�=DLj=DL�=DMj=DM�=DNj=DN�=DOj=DO�=DPj=DP�=DQj=DQ�=DRj=DR�=DSj=DS�=DTj=DT�=DUj=DU�=DVj=DV�=DWj=DW�=DXj=DX�=DYj=DY�=DZj=DZ�=D[j=D[�=D\j=D\�=D]j=D]�=D^j=D^�=D_j=D_�=D`j=D`�=Daj=Da�=Dbj=Db�=Dcj=Dc�=Ddj=Dd�=Dej=De�=Dfj=Df�=Dgj=Dg�=Dhj=Dh�=Dij=Di�=Djj=Dj�=Dkj=Dk�=Dlj=Dl�=Dmj=Dm�=Dnj=Dn�=Doj=Do�=Dpj=Dp�=Dqj=Dq�=Drj=Dr�=Dsj=Ds�=Dtj=Dt�=Duj=Du�=Dvj=Dv�=Dwj=Dw�=Dxj=Dx�=Dyj=Dy�=Dzj=Dz�=D{j=D{�=D|j=D|�=D}j=D}�=D~j=D~�=Dj=D�=D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uDµD��D�5D�uDõD��D�5D�uDĵD��D�5D�uDŵD��D�5D�uDƵD��D�5D�uDǵD��D�5D�uDȵD��D�5D�uDɵD��D�5D�uDʵD��D�5D�uD˵D��D�5D�uD̵D��D�5D�uD͵D��D�5D�uDεD��D�5D�uDϵD��D�5D�uDеD��D�5D�uDѵD��D�5D�uDҵD��D�5D�uDӵD��D�5D�uDԵD��D�5D�uDյD��D�5D�uDֵD��D�5D�uD׵D��D�5D�uDصD��D�5D�uDٵD��D�5D�uDڵD��D�5D�uD۵D��D�5D�uDܵD��D�5D�uDݵD��D�5D�uD޵D��D�5D�uDߵD��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD��D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�xRD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�p�A�dZA�`BA�^5A�\)A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�`BA�`BA�`BAדuA��mA�?}A�|�A�/A�"�A�l�A˰!A���Aɧ�A�9XAǏ\A��
AƑhA�^5A�ȴAÃA��/A��#A��
A��PA��A���A�oA��A�dZA�;dA�M�A��HA���A��A���A��A���A��+A�G�A���A���A�/A�7LA��wA�S�A�A�-A�~�A���A�A�A�+A���A�hsA��PA�
=A��A�VA���A���A�-A��wA�p�A��TA�7LA��-A�C�A��A�^5A�;dA���A�7LA���A��uA��A���A��^A�
=A�C�A�XA��A�O�A�"�A�VA�A�A���A���A��
A��A� �A�l�A�Q�A��
A���A�dZA��A�1A~��A{t�Azr�Ax1Av$�AsC�Apv�AmAk&�Ai�^Ai;dAhM�Af�AdĜAb^5A`�A_\)A\5?AXA�AU�;AT�/AS�hAR �APv�ANJAL��AL�AL��AK�AJ(�AI&�AH~�AG`BAF��AF�\AEp�AC7LAB~�AAdZA?��A<��A;&�A8�`A8I�A8VA5�A4�A3�A3�mA3�;A3dZA3�A2z�A1A1?}A0bNA/��A.��A.~�A-�mA,ȴA+�hA*A�A)t�A(E�A'%A%�A#|�A"{A!��A A�A��A��AƨA%A�
A�A^5A�-A33A�A�AVA��A�RA��A��AA�PA+A�A��A�A�jA�A"�AA�A
��A	C�A&�AQ�AƨA�A �A�PA?}A"�A%A��A�@���@�33@��-@���@�~�@�^@�p�@�?}@�&�@��`@�bN@@�M�@�z�@�v�@��@�~�@�-@�(�@�V@��@���@݁@ۍP@ڸR@��@؃@���@��@� �@җ�@�M�@�X@Гu@��@�@Ο�@��@ͺ^@�G�@̼j@�A�@ˮ@��y@��@��@��@���@ȼj@Ȭ@ɑh@ț�@�l�@��@��@őh@��`@�Ĝ@öF@���@��R@�V@��^@��@���@�I�@��@�1@��H@�$�@���@�7L@���@��D@��@�t�@�dZ@�K�@�o@���@��R@�\)@��@��@��@�dZ@�
=@�v�@��h@��@�z�@�b@�\)@���@��R@���@�E�@�=q@�J@�@���@��@�/@�&�@��@��/@�A�@��F@�dZ@�"�@��@���@��@���@��7@�p�@��@�Ĝ@� �@��@��;@��F@�l�@��H@�~�@�^5@�{@��^@�hs@�?}@���@�A�@� �@�b@�1@��@��w@�S�@��@�o@���@��R@��+@�n�@�M�@�5?@�J@��@���@��7@�x�@�hs@�X@��@��9@�Z@�1@�ƨ@���@��@�K�@�"�@��R@��\@�ff@�E�@��@���@��T@��#@�@�x�@�O�@�?}@�&�@���@���@�I�@�b@��m@�ƨ@���@�K�@�
=@�ȴ@���@�v�@�^5@�E�@�$�@��#@���@�p�@�&�@���@��@�j@�Q�@�A�@�9X@�9X@�(�@��@���@�ƨ@���@�l�@�;d@�
=@��!@�V@��@���@�hs@�G�@�%@��@�1'@���@���@��P@�S�@�C�@�33@��@���@���@���@���@�n�@��@�J@���@���@��h@��7@�`B@���@��@�Q�@�1@���@�|�@�S�@�33@�"�@�"�@�o@�@�ȴ@���@�E�@���@��T@���@���@�x�@�X@�7L@��@��/@�Ĝ@��j@�r�@�1'@�(�@�b@�  @��@�ƨ@��@�|�@�t�@�l�@�"�@��y@�V@�$�@��@�J@���@���@��7@�p�@�X@�G�@�/@��@�%@��/@���@�z�@�A�@�(�@�1@���@���@�ƨ@�ƨ@��@�l�@��@��@��!@�v�@�J@��#@���@�O�@���@�j@���@���@��@�S�@��R@�$�@���@���@��7@�x�@�`B@��/@�Ĝ@��u@�j@��@�@�;@K�@~�R@~��@~ff@~{@}p�@|�@|9X@{��@z��@z-@z-@z-@y�@yX@xr�@w�;@w�@v��@v5?@u��@t��@t�@tj@s��@sC�@r��@q��@q&�@p�`@p��@pA�@o�;@o�w@o�@o��@oK�@o;d@o�@o�@nȴ@n@m�-@m�h@mO�@l��@l9X@k�@kt�@j�H@j�@i��@ix�@iG�@h��@h�u@hbN@h �@h  @g�;@g��@g�P@g\)@f�R@e��@ep�@e?}@d�@d�@d1@ct�@cC�@cC�@c33@c@b��@b=q@a��@ax�@a%@`�9@`bN@_�@_|�@_l�@_K�@^��@]@]�-@]�h@]?}@\�j@\�@[��@[�
@[S�@Z��@Z�!@Z��@Z�\@Z~�@Z=q@Z-@Y�@Y�#@Y��@YG�@Y7L@Y7L@Y%@XĜ@X�@W�;@W\)@W;d@V�@VE�@U@U/@T�/@T�D@T1@S��@R�@R��@RM�@RJ@Q��@QX@Q&�@P��@PA�@O�@O��@O|�@O\)@O\)@OK�@O;d@O;d@O�@N�@N$�@M�-@M`B@L��@Lz�@L�@K�m@Kƨ@K�@KS�@K"�@K@J�!@J-@I�@I�@I��@I��@IG�@H��@H��@H  @GK�@F�@Fv�@FV@FE�@F5?@F$�@F@E�@D��@D�/@D�j@D��@DZ@C��@Ct�@B�@Bn�@A�#@A�7@A�7@Ahs@AX@AX@@Ĝ@@r�@@ �@?�@?|�@?\)@?+@?
=@>�y@>�R@>��@>v�@>$�@>@=�@=�h@=p�@=O�@=/@=/@=�@=V@<��@<�/@<��@<�@<z�@<Z@;�
@;��@;t�@;"�@:��@:J@9��@97L@9�@8��@8�`@8Ĝ@8�u@8A�@7�@7�P@7l�@7;d@7�@7�@7�@7�@7
=@6ȴ@6�+@6E�@5�@5@5��@5�h@5�@4I�@3ƨ@3S�@2�H@2�\@2^5@1��@1��@1&�@0��@0�9@0�@01'@0b@0  @/�@/��@/�@/�P@/K�@.�y@.��@.�+@.5?@-��@-@-@-�-@-�h@,��@,��@,j@,I�@,9X@+��@+�m@+�
@+�@+"�@*��@*��@*~�@*^5@*=q@)�#@)��@)G�@)�@(��@(��@(�u@(bN@(Q�@(b@'�@'|�@&��@&�@&ȴ@&ȴ@&��@&ff@%�T@%�h@%?}@$��@$�@$��@$��@$z�@$j@$Z@$I�@$1@#�m@#�
@#�@#C�@"��@"^5@!��@!��@!��@!�@!��@!G�@ ��@ ��@ Ĝ@ �u@ bN@ Q�@  �@  �@   @�w@+@
=@�@v�@$�@��@�-@O�@�j@��@�D@z�@Z@9X@�
@�F@�F@��@�@dZ@S�@o@�H@��@��@��@�!@��@�\@^5@-@�@��@��@&�@��@��@Ĝ@r�@Q�@ �@b@�@�w@��@��@l�@\)@K�@+@�y@�+@E�@5?@$�@{@@�T@��@O�@/@V@�@��@��@��@�F@��@��@t�@dZ@dZ@C�@C�@@��@�!@n�@�@�#@�#@��@��@��@hs@&�@�@��@Ĝ@�u@�@bN@1'@  @��@�w@��@K�@K�@;d@+@�y@�R@�R@��@��@��@�+@5?@$�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�p�A�dZA�`BA�^5A�\)A�^5A�^5A�^5A�`BA�`BA�`BA�`BA�`BA�`BA�`BAדuA��mA�?}A�|�A�/A�"�A�l�A˰!A���Aɧ�A�9XAǏ\A��
AƑhA�^5A�ȴAÃA��/A��#A��
A��PA��A���A�oA��A�dZA�;dA�M�A��HA���A��A���A��A���A��+A�G�A���A���A�/A�7LA��wA�S�A�A�-A�~�A���A�A�A�+A���A�hsA��PA�
=A��A�VA���A���A�-A��wA�p�A��TA�7LA��-A�C�A��A�^5A�;dA���A�7LA���A��uA��A���A��^A�
=A�C�A�XA��A�O�A�"�A�VA�A�A���A���A��
A��A� �A�l�A�Q�A��
A���A�dZA��A�1A~��A{t�Azr�Ax1Av$�AsC�Apv�AmAk&�Ai�^Ai;dAhM�Af�AdĜAb^5A`�A_\)A\5?AXA�AU�;AT�/AS�hAR �APv�ANJAL��AL�AL��AK�AJ(�AI&�AH~�AG`BAF��AF�\AEp�AC7LAB~�AAdZA?��A<��A;&�A8�`A8I�A8VA5�A4�A3�A3�mA3�;A3dZA3�A2z�A1A1?}A0bNA/��A.��A.~�A-�mA,ȴA+�hA*A�A)t�A(E�A'%A%�A#|�A"{A!��A A�A��A��AƨA%A�
A�A^5A�-A33A�A�AVA��A�RA��A��AA�PA+A�A��A�A�jA�A"�AA�A
��A	C�A&�AQ�AƨA�A �A�PA?}A"�A%A��A�@���@�33@��-@���@�~�@�^@�p�@�?}@�&�@��`@�bN@@�M�@�z�@�v�@��@�~�@�-@�(�@�V@��@���@݁@ۍP@ڸR@��@؃@���@��@� �@җ�@�M�@�X@Гu@��@�@Ο�@��@ͺ^@�G�@̼j@�A�@ˮ@��y@��@��@��@���@ȼj@Ȭ@ɑh@ț�@�l�@��@��@őh@��`@�Ĝ@öF@���@��R@�V@��^@��@���@�I�@��@�1@��H@�$�@���@�7L@���@��D@��@�t�@�dZ@�K�@�o@���@��R@�\)@��@��@��@�dZ@�
=@�v�@��h@��@�z�@�b@�\)@���@��R@���@�E�@�=q@�J@�@���@��@�/@�&�@��@��/@�A�@��F@�dZ@�"�@��@���@��@���@��7@�p�@��@�Ĝ@� �@��@��;@��F@�l�@��H@�~�@�^5@�{@��^@�hs@�?}@���@�A�@� �@�b@�1@��@��w@�S�@��@�o@���@��R@��+@�n�@�M�@�5?@�J@��@���@��7@�x�@�hs@�X@��@��9@�Z@�1@�ƨ@���@��@�K�@�"�@��R@��\@�ff@�E�@��@���@��T@��#@�@�x�@�O�@�?}@�&�@���@���@�I�@�b@��m@�ƨ@���@�K�@�
=@�ȴ@���@�v�@�^5@�E�@�$�@��#@���@�p�@�&�@���@��@�j@�Q�@�A�@�9X@�9X@�(�@��@���@�ƨ@���@�l�@�;d@�
=@��!@�V@��@���@�hs@�G�@�%@��@�1'@���@���@��P@�S�@�C�@�33@��@���@���@���@���@�n�@��@�J@���@���@��h@��7@�`B@���@��@�Q�@�1@���@�|�@�S�@�33@�"�@�"�@�o@�@�ȴ@���@�E�@���@��T@���@���@�x�@�X@�7L@��@��/@�Ĝ@��j@�r�@�1'@�(�@�b@�  @��@�ƨ@��@�|�@�t�@�l�@�"�@��y@�V@�$�@��@�J@���@���@��7@�p�@�X@�G�@�/@��@�%@��/@���@�z�@�A�@�(�@�1@���@���@�ƨ@�ƨ@��@�l�@��@��@��!@�v�@�J@��#@���@�O�@���@�j@���@���@��@�S�@��R@�$�@���@���@��7@�x�@�`B@��/@�Ĝ@��u@�j@��@�@�;@K�@~�R@~��@~ff@~{@}p�@|�@|9X@{��@z��@z-@z-@z-@y�@yX@xr�@w�;@w�@v��@v5?@u��@t��@t�@tj@s��@sC�@r��@q��@q&�@p�`@p��@pA�@o�;@o�w@o�@o��@oK�@o;d@o�@o�@nȴ@n@m�-@m�h@mO�@l��@l9X@k�@kt�@j�H@j�@i��@ix�@iG�@h��@h�u@hbN@h �@h  @g�;@g��@g�P@g\)@f�R@e��@ep�@e?}@d�@d�@d1@ct�@cC�@cC�@c33@c@b��@b=q@a��@ax�@a%@`�9@`bN@_�@_|�@_l�@_K�@^��@]@]�-@]�h@]?}@\�j@\�@[��@[�
@[S�@Z��@Z�!@Z��@Z�\@Z~�@Z=q@Z-@Y�@Y�#@Y��@YG�@Y7L@Y7L@Y%@XĜ@X�@W�;@W\)@W;d@V�@VE�@U@U/@T�/@T�D@T1@S��@R�@R��@RM�@RJ@Q��@QX@Q&�@P��@PA�@O�@O��@O|�@O\)@O\)@OK�@O;d@O;d@O�@N�@N$�@M�-@M`B@L��@Lz�@L�@K�m@Kƨ@K�@KS�@K"�@K@J�!@J-@I�@I�@I��@I��@IG�@H��@H��@H  @GK�@F�@Fv�@FV@FE�@F5?@F$�@F@E�@D��@D�/@D�j@D��@DZ@C��@Ct�@B�@Bn�@A�#@A�7@A�7@Ahs@AX@AX@@Ĝ@@r�@@ �@?�@?|�@?\)@?+@?
=@>�y@>�R@>��@>v�@>$�@>@=�@=�h@=p�@=O�@=/@=/@=�@=V@<��@<�/@<��@<�@<z�@<Z@;�
@;��@;t�@;"�@:��@:J@9��@97L@9�@8��@8�`@8Ĝ@8�u@8A�@7�@7�P@7l�@7;d@7�@7�@7�@7�@7
=@6ȴ@6�+@6E�@5�@5@5��@5�h@5�@4I�@3ƨ@3S�@2�H@2�\@2^5@1��@1��@1&�@0��@0�9@0�@01'@0b@0  @/�@/��@/�@/�P@/K�@.�y@.��@.�+@.5?@-��@-@-@-�-@-�h@,��@,��@,j@,I�@,9X@+��@+�m@+�
@+�@+"�@*��@*��@*~�@*^5@*=q@)�#@)��@)G�@)�@(��@(��@(�u@(bN@(Q�@(b@'�@'|�@&��@&�@&ȴ@&ȴ@&��@&ff@%�T@%�h@%?}@$��@$�@$��@$��@$z�@$j@$Z@$I�@$1@#�m@#�
@#�@#C�@"��@"^5@!��@!��@!��@!�@!��@!G�@ ��@ ��@ Ĝ@ �u@ bN@ Q�@  �@  �@   @�w@+@
=@�@v�@$�@��@�-@O�@�j@��@�D@z�@Z@9X@�
@�F@�F@��@�@dZ@S�@o@�H@��@��@��@�!@��@�\@^5@-@�@��@��@&�@��@��@Ĝ@r�@Q�@ �@b@�@�w@��@��@l�@\)@K�@+@�y@�+@E�@5?@$�@{@@�T@��@O�@/@V@�@��@��@��@�F@��@��@t�@dZ@dZ@C�@C�@@��@�!@n�@�@�#@�#@��@��@��@hs@&�@�@��@Ĝ@�u@�@bN@1'@  @��@�w@��@K�@K�@;d@+@�y@�R@�R@��@��@��@�+@5?@$�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BZBZBZBZBZBZBZBZBZBZBZB[#B[#B[#BcTB�BB\B[#B{�B}�Bw�BhsBT�BH�BG�BR�Be`Bm�Bw�B{�B�=B��B��B��B��B��B��B�uB�{B��B��B�B�3B�FB�XBB��B��B��B��B�B�)B�BB�BB�B��B�
B�B��B��B�B��B��B�jB�LB�-B��B��B��B��B�bB�+By�B_;BS�BQ�B<jB0!B�BBB��B�NB�?B�uB�=B�DB}�Bu�Bz�Bl�B`BBXB0!B�B1B
��B
�B
�B
��B
�RB
�!B
��B
��B
�bB
�B
t�B
_;B
C�B
.B
#�B
{B

=B	�B	�B	�qB	�dB	�9B	�B	��B	��B	�7B	x�B	l�B	bNB	O�B	<jB	/B	'�B	 �B	�B	�B	bB	JB	uB	oB	�B	{B	bB	JB	1B	B	B	B��B��B�B�sB�5B�
B��B��B�B��B�}B�}B��B��B��B��B��B�}B�qB�LB�9B�B��B��B��B��B��B��B��B�oB�bB�7B�+B�B�B{�By�Bv�Bt�Bs�Bp�Bo�Bn�Bn�Bn�Bm�Bn�Bn�Bm�BiyBhsBdZBdZBdZBcTBbNBaHB_;B^5B_;B]/B\)B]/B]/B\)B]/B^5BbNBbNBcTBcTBcTBbNBgmBdZBgmBffBiyBdZBdZBdZBdZBdZBdZBcTBcTBdZBiyBiyBiyBiyBhsBhsBhsBiyBjBjBl�Bl�Bl�Bl�Bm�Bs�Bt�Bx�B{�B}�B}�B� B� B� B� B�B�B�B�B�1B�DB�PB�PB�bB�bB�hB��B��B��B�}B��BB�LB�RB�qB�wB�^B�'B�'B�'B�'B�-B�FB�dB�wB��BÖBŢBɺBɺBɺB��B��B��B��B�B�5B�ZB�B��B��B��B��B��B��B��B��B��B��B	B	B	+B	DB	oB	oB	�B	�B	�B	�B	�B	 �B	!�B	$�B	)�B	0!B	49B	7LB	9XB	;dB	@�B	D�B	E�B	F�B	J�B	M�B	S�B	T�B	VB	VB	YB	_;B	cTB	e`B	hsB	jB	l�B	m�B	t�B	w�B	x�B	x�B	x�B	y�B	{�B	~�B	�B	�B	�B	�B	�+B	�7B	�DB	�JB	�PB	�VB	�VB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�9B	�9B	�?B	�FB	�XB	�^B	�dB	�jB	�jB	�qB	�}B	B	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�B	�B	�#B	�#B	�/B	�;B	�HB	�HB	�NB	�NB	�ZB	�`B	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
PB
VB
\B
bB
hB
hB
oB
oB
oB
uB
{B
{B
{B
{B
{B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
)�B
+B
,B
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
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
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
49B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
49B
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
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
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
<jB
<jB
<jB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
?}B
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
L�B
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
N�B
N�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
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
S�B
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
ZB
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
iyB
iyB
iyB
iyB
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
k�B
l�B
l�B
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
q�B
q�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
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
w�B
w�B
v�B
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
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BZQBZ7BZ7BZ7BZ7BZ7BZ7BZ7BZ7BZ7BZ7B[=B[qB[�BezB�hBMB_�BB�Bz�BjKBW?BKxBJ�BT�Bf�BncBx�B~BB�pB�pB�!B�@B��B��B�QB�B��B�FB�sB��B�9B�2B��BðB��B��B�VBϫB��B�/B�B�B�?B�&B�yBچBյB֡BרB��B��B��B��B�%B��B�zB��B��B��B�XB|�B`�BV9BT�B>BB3MB!|B9B{B�>B�B�B��B�^B�jB�BxB}<Bo�BcB\)B2�BB
	B
�VB
�2B
�OB
�B
��B
�B
�$B
��B
�TB
��B
x�B
cTB
G+B
0!B
&�B
sB
�B	�aB	��B	� B	�B	�?B	��B	��B	��B	�B	{0B	oiB	ffB	TaB	?.B	0�B	)�B	#B	B	QB	�B	�B	,B	FB	eB	�B	�B	�B		B	B	�B	{B�>B��B�hB�B��B�B��B�B��B͟B� B��B��B�UB�[BªBªB�iB��B�RB�tB��B�>B��B��B��B�B�_B�yB��B��B�B�fB�B�9B}�B|6BxBv`BuBq�Bp�Bo�BoOBo5Bn}Bo�BpBoBj�Bi�BeBeBeBdZBc�Bb�B`�B_�B`�B_VB^�B_�B^jB]IB^jB_�Bc Bb�Bc�Bc�BdZBdZBj�Bf�Bi*BiBkkBeBd�Bd�Bd�Bd�BeBdZBd�Be�BkBk6Bj�BjeBi�Bi�Bi�Bj�Bk�Bk�Bm]Bm�Bm�Bm�Bn�Bu%Bu�ByXB|�B~�B~�B��B��B��B��B��B��B��B��B�B��B��B��B�B��B��B�gB�B��B��B�jBāB�B�	B��B�B�B��B��B��B��B��B��B��B�}B�;B��B�YB�=B�#B�rB�6B�B�4B�uB�B�OB�@B�B��B��B��B�ZB��B��B��B�rB�xB��B	oB	{B	�B	�B	�B	�B	�B	�B	B	B	�B	!B	"NB	%zB	*B	0�B	4�B	7�B	9�B	<B	AB	D�B	FB	G+B	K)B	NpB	TFB	UMB	VSB	V�B	Y�B	_�B	c�B	e�B	h�B	j�B	l�B	nB	u%B	xB	y$B	y$B	y$B	z*B	|jB	cB	�UB	�[B	�{B	�mB	�zB	�lB	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�&B	�FB	�LB	�sB	�0B	�kB	�]B	�]B	�iB	�oB	�[B	�|B	��B	��B	��B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�0B	�"B	�HB	�NB	�TB	�@B	�@B	�,B	�MB	�MB	�9B	�SB	�SB	�9B	�SB	�YB	�yB	�kB	�qB	یB	ݘB	ߤB	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	� B	��B	��B	��B	��B	�-B	�B	�9B	�%B	�+B	�B	�B	�	B	�	B	�	B	�$B	�$B	�$B	�*B	�DB	�6B	�<B	�VB	�(B	�(B	�(B	�HB	�HB	�cB
UB
;B
oB
{B
gB
SB
mB
tB
zB
�B
	�B
	lB

�B

�B
�B
�B
�B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
�B
�B
+B
B
B
�B
�B
�B
�B
1B
7B
�B
	B
	B
�B
B
)B
�B
B
�B
B
B
B
B
B
B
B
 'B
 'B
!-B
!-B
!HB
"NB
"4B
!�B
"B
"B
"4B
"4B
# B
$ZB
$@B
$@B
$&B
$&B
%,B
%,B
%FB
%`B
&2B
&fB
'RB
(>B
($B
(XB
)*B
*0B
*0B
*KB
*0B
+6B
+QB
+6B
*KB
+QB
,WB
,=B
,=B
,WB
-wB
-wB
-]B
-]B
-wB
-wB
.cB
.cB
/OB
/iB
/OB
/OB
0oB
0oB
0UB
0oB
0oB
0�B
1�B
2|B
2|B
2|B
2�B
2�B
3�B
3�B
3hB
3�B
3�B
3�B
4�B
3�B
3�B
3�B
4�B
4�B
4�B
5tB
5tB
5�B
4�B
5�B
6zB
6�B
6�B
6�B
6�B
7�B
7�B
7�B
8�B
8�B
8lB
8�B
8�B
8�B
9�B
9�B
9�B
9�B
9�B
:�B
9�B
9�B
:�B
:�B
:�B
;�B
;�B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
?�B
?�B
?�B
?�B
@�B
@ B
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
EB
FB
FB
E�B
F�B
F�B
F�B
F�B
F�B
GB
HB
G�B
G�B
G�B
G�B
G�B
HB
IB
IB
J#B
KB
J�B
J�B
KB
KB
K)B
LB
LB
LB
MB
MB
MB
MB
MB
MB
MB
MB
MB
N"B
N"B
NB
N"B
N"B
N"B
OB
OB
N"B
OB
OB
OB
O(B
OB
OB
OBB
OB
P.B
PB
PHB
PHB
Q4B
Q4B
R:B
R B
R:B
R:B
R B
R B
R:B
S&B
S@B
S@B
S@B
S&B
S&B
S&B
S&B
S@B
T,B
T,B
TFB
TFB
U2B
UMB
TFB
U�B
VmB
VSB
WYB
W?B
WYB
X_B
XyB
X_B
XEB
YeB
YeB
YeB
YKB
Z7B
ZQB
ZQB
ZkB
ZkB
ZQB
Z�B
[qB
[qB
[qB
[qB
\]B
\CB
\xB
\xB
\�B
\]B
\]B
]dB
]~B
]dB
]dB
]~B
^�B
^�B
^�B
_�B
_�B
_pB
_�B
_�B
_pB
_�B
`vB
`vB
`vB
`vB
`�B
a�B
a�B
a�B
a|B
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
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
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
g�B
g�B
g�B
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
sB
r�B
s�B
s�B
tB
s�B
s�B
tB
s�B
s�B
s�B
s�B
tB
t�B
uB
t�B
t�B
t�B
uB
t�B
t�B
u�B
vB
vB
u�B
u�B
vB
u�B
v�B
v�B
v�B
v�B
wB
xB
xB
wB
xB
xB
w�B
xB
xB
xB
xB
xB
y	B
y	B
y	B
y	111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.34(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201707070038512017070700385120170707003851201806221315442018062213154420180622131544201804050717362018040507173620180405071736  JA  ARFMdecpA19c                                                                20170703063513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170702213514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170702213515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170702213516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170702213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170702213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170702213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170702213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170702213517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170702213517                      G�O�G�O�G�O�                JA  ARUP                                                                        20170702220846                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170703153549  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20170706153851  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170706153851  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221736  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041544  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211516                      G�O�G�O�G�O�                