CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-07-02T00:35:18Z creation;2016-07-02T00:35:20Z conversion to V3.1;2019-12-19T08:32:56Z update;     
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `l   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �<   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20160702003518  20200116201515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_013                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @׸5牬 1   @׸6����@2R�W����d�F�]d1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D���D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @p��@��@��A�\A:�\AZ�\Az�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B&��B.=qB6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B��B�Q�B�Q�B�Q�B�Q�B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C��CC��C��C	��C��C��C��C�\C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�ǮC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�ǮC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D j=D �=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=D	j=D	�=D
j=D
�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=D j=D �=D!j=D!�=D"j=D"�=D#j=D#�=D$j=D$�=D%j=D%�=D&j=D&�=D'j=D'�=D(j=D(�=D)j=D)�=D*j=D*�=D+j=D+�=D,j=D,�=D-j=D-�=D.j=D.�=D/j=D/�=D0j=D0�=D1j=D1�=D2j=D2�=D3j=D3�=D4j=D4�=D5j=D5�=D6j=D6�=D7j=D7�=D8j=D8�=D9j=D9�=D:j=D:�=D;j=D;�=D<j=D<�=D=j=D=�=D>j=D>�=D?j=D?�=D@j=D@�=DAj=DA�=DBj=DB�=DCj=DC�=DDj=DD�=DEj=DE�=DFj=DF�=DGj=DG�=DHj=DH�=DIj=DI�=DJj=DJ�=DKj=DK�=DLj=DL�=DMj=DM�=DNj=DN�=DOj=DO�=DPj=DP�=DQj=DQ�=DRj=DR�=DSj=DS�=DTj=DT�=DUj=DU�=DVj=DV�=DWj=DW�=DXj=DX�=DYj=DY�=DZj=DZ�=D[j=D[�=D\j=D\�=D]j=D]�=D^j=D^�=D_j=D_�=D`j=D`�=Daj=Da�=Dbj=Db�=Dcj=Dc�=Ddj=Dd�=Dej=De�=Dfj=Df�=Dgj=Dg�=Dhj=Dh�=Dij=Di�=Djj=Dj�=Dkj=Dk�=Dlj=Dl�=Dmj=Dm�=Dnj=Dn�=Doj=Do�=Dpj=Dp�=Dqj=Dq�=Drj=Dr�=Dsj=Ds�=Dtj=Dt�=Duj=Du�=Dvj=Dv�=Dwj=Dw�=Dxj=Dx�=Dyj=Dy�=Dzj=Dz�=D{j=D{�=D|j=D|�=D}j=D}�=D~j=D~�=Dj=D�=D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�xRD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uDµD��D�5D�uDõD��D�5D�uDĵD��D�5D�uDŵD��D�5D�uDƵD��D�5D�uDǵD��D�5D�uDȵD��D�5D�uDɵD��D�5D�uDʵD��D�5D�uD˵D��D�5D�uD̵D��D�5D�uD͵D��D�5D�uDεD��D�5D�uDϵD��D�5D�uDеD��D�5D�uDѵD��D�5D�uDҵD���D�5D�uDӵD��D�5D�uDԵD��D�5D�uDյD��D�5D�uDֵD��D�5D�uD׵D��D�5D�uDصD��D�5D�uDٵD��D�5D�uDڵD��D�5D�uD۵D��D�5D�uDܵD��D�5D�uDݵD��D�5D�uD޵D��D�5D�uDߵD��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D���D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD��D��D�5D�uD�D��D�5D�uD�D��D�5D�q�D�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD�D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�5D�uD��D��D�8RD�hR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�;dA�=qA�=qA�?}A�A�A�E�A�E�A�E�A�E�A�G�A�I�A�I�A�K�A�M�A�M�A�O�A�O�A�Q�A�7LA�-A�"�A�+A��A��HA��`A��A���Aӗ�A�l�A�`BA�\)A�ZA�XA�I�A��A�  A��A���AҴ9Aҧ�Aҝ�AґhA���A���A�x�A���A��A��A�bNA�%A���A��A�7LA���A�XAş�A�O�A��mAę�A�/A���Aé�AÛ�A�ȴAÝ�Aº^A�O�A��A���A�(�A��PA�v�A�O�A� �A��\A�n�A���A��-A�G�A�9XA��A�A�A�\)A���A� �A�x�A���A��A���A��hA�XA�7LA���A�A�A��RA�z�A�r�A�
=A�=qA��;A�VA��uA�ĜA��^A��#A�x�A��\A�;dA�/A}K�Az$�Aw�Av�As�#ArM�Ap��Am��Ak��AkVAh��Ag�hAf�DAdAbE�A`9XA^��A[��A[+AZ�AWhsAU�wAS�TAR�/ARM�AP�AOp�AN�yANM�AM\)ALffAK%AI�AF��AD^5AB�A@I�A>�A=��A<��A;�^A:�A9��A8�A8�A6n�A6$�A5�A5�7A4�9A3�-A2(�A17LA/"�A-��A+dZA+&�A*�`A)VA(1'A&{A%+A$��A#��A"=qA!�A!%A jA7LA��A�yA5?A�mA��A�7A7LA+AhsA�FAK�AbNAdZA�+A"�AbNAx�Ar�A1'A �AJA�AA	�wA	`BA	"�A	AĜA^5A�A��A5?AȴA�#A��A\)A��A��A �A�^AVA��A ĜA -@�-@�1'@�z�@��;@��
@�b@�%@�hs@�%@�r�@���@��@���@�b@�^@�ȴ@�7@��@�@�S�@�
=@�^5@�X@�O�@�X@�G�@睲@��@��T@���@�@�Ĝ@�Z@�t�@�hs@�Q�@�b@��
@�o@ݲ-@�9X@ە�@�;d@ڸR@ى7@�M�@��@���@���@���@�^5@���@�x�@ԃ@�v�@�@ͺ^@�"�@��m@���@�"�@�o@Ͳ-@��#@��T@�G�@�9X@˾w@���@�ȴ@ʟ�@ǅ@ǝ�@�dZ@Ǖ�@ƸR@�v�@�@ź^@�  @�X@��T@���@�r�@���@�9X@���@��y@��@���@��@� �@�(�@�@�V@��j@�@��@���@��@�b@�
=@���@���@�M�@�b@�j@��H@�+@���@�o@�^5@��R@�o@��F@�C�@�ȴ@���@��@���@��+@�E�@�@��9@��@��D@�bN@���@��7@�hs@��@��^@�M�@�S�@�;d@��H@�@��h@�/@��u@���@��y@���@���@���@��-@��#@���@��7@��h@�G�@���@��j@�z�@�1@�dZ@�
=@��@�^5@��T@��/@��
@���@��P@�t�@���@�E�@���@�O�@���@�1'@��@�
=@�ff@��T@��-@��@�I�@�Q�@���@���@�j@�1@�t�@���@��T@���@�~�@�-@�X@�Ĝ@��@��/@��@��@�1'@��@���@�t�@�"�@�n�@�@���@���@���@���@��h@�7L@�%@��j@�Q�@�9X@�(�@� �@�b@���@�V@�5?@�{@�5?@��@���@�x�@��`@�1@�o@�A�@���@��@��u@��@���@��@�dZ@�@�ȴ@���@�~�@��+@�^5@�&�@���@�7L@�V@���@���@��D@��u@��u@��@��w@�l�@�+@�"�@�ȴ@���@��!@��@���@��\@��@�G�@�&�@��j@��/@��j@��D@�r�@�bN@�(�@�1@��@��w@�t�@�;d@��+@�5?@�J@�{@���@��-@�&�@�%@�%@��/@��9@��u@�Z@��;@��
@��
@���@��@���@��+@�v�@�M�@�=q@�5?@�-@��T@��h@��@�p�@�O�@�7L@��`@��D@��@���@�t�@�dZ@�+@��!@�ff@�M�@�^5@�v�@�5?@�-@��7@�hs@�G�@�?}@�&�@�&�@��@���@��`@��j@��@�j@�I�@�(�@�@\)@~�y@~��@~V@}��@|�@|�@|j@|I�@|Z@|�@|�D@|z�@|I�@|�@{��@{C�@{o@z^5@y7L@wK�@v��@v�+@v�+@vE�@v{@u��@u`B@u?}@u`B@uV@t��@t�j@t�j@t�j@tj@sƨ@r��@q��@qX@q��@q%@o��@o
=@o;d@o\)@o
=@n��@nv�@n$�@m�@l��@kC�@j=q@i�@i��@h��@h�9@hbN@h1'@g;d@f�y@f�+@f@e�@e?}@d�@c�m@cS�@c"�@b�@a�#@aX@a&�@a�@`Ĝ@`Q�@` �@`  @_�@_;d@^��@^��@^5?@^@]��@]@]��@]`B@\�@\j@[t�@Z��@Z~�@ZM�@Y��@Y�@XĜ@X��@Xr�@X1'@W�;@W�P@WK�@V��@V�R@Vv�@U��@U�-@U`B@T��@Tz�@T(�@T�@T1@S�m@St�@So@R�@R�!@R~�@Rn�@RM�@R^5@R=q@RJ@Q��@Q�@Pr�@P  @O�@O��@O�@O�P@O|�@O\)@Nȴ@Nff@N5?@M��@Mp�@L��@LI�@K�
@K��@K�@Kt�@KdZ@Ko@Jn�@J�@I��@I�7@I�@I�@H��@H�9@HbN@HA�@H �@Hb@G��@G\)@F��@FV@F{@E�T@E@E�@E/@EV@D��@Dj@D9X@D(�@C�
@C"�@B�@B�@B�!@B�\@Bn�@BM�@BM�@B=q@A��@Ahs@@��@@r�@@r�@@bN@?�;@?�P@?;d@>�@>��@>E�@>@=�@=�-@=/@<�@<��@<�@<z�@<9X@;��@;��@;�@;S�@;"�@:�@:�\@:^5@9��@9�7@97L@8�`@8Ĝ@8�u@8A�@8 �@8 �@7�w@7\)@7�@6ȴ@6V@5��@5`B@5?}@5V@4�/@49X@4�@3��@3��@41@41@41@3��@3��@333@1�@0��@0 �@/�w@/l�@/;d@/
=@.�R@.��@.V@.@-�@-V@,��@,�j@,1@+dZ@+@*�H@*��@*�\@*J@)��@)��@)hs@)hs@)X@)7L@(�`@(��@(�@(Q�@(  @'�@'|�@&��@&E�@%�@%?}@%�@$�@$I�@#��@#"�@"��@"-@!��@!X@ ��@ ��@ Q�@ 1'@   @�P@;d@�y@ff@@�-@?}@��@�/@�D@(�@�@1@1@��@�m@ƨ@�F@��@�@dZ@C�@@�!@�\@~�@~�@J@X@��@��@Ĝ@�u@r�@r�@Q�@Q�@1'@ �@ �@ �@  @�;@�P@|�@\)@��@ȴ@�R@��@�+@E�@{@�T@�-@O�@/@�@V@z�@1@�F@�@dZ@o@��@�\@=q@J@��@��@x�@&�@�`@��@��@�@�@�@r�@Q�@A�@1'@  @��@l�@\)@;d@+@�@�+@ff@V@5?@{@�@�T@�-@�h@�h@�@�@V@V@V@�@�/@�j@z�@9X@1@��@�m@��@"�@"�@"�@"�@o@
��@
��@
�\@
^5@
M�@
=q@
=q@
J@	��@	X@	�@��@Ĝ@�9@�@Q�@1'@ �@ �@b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�;dA�=qA�=qA�?}A�A�A�E�A�E�A�E�A�E�A�G�A�I�A�I�A�K�A�M�A�M�A�O�A�O�A�Q�A�7LA�-A�"�A�+A��A��HA��`A��A���Aӗ�A�l�A�`BA�\)A�ZA�XA�I�A��A�  A��A���AҴ9Aҧ�Aҝ�AґhA���A���A�x�A���A��A��A�bNA�%A���A��A�7LA���A�XAş�A�O�A��mAę�A�/A���Aé�AÛ�A�ȴAÝ�Aº^A�O�A��A���A�(�A��PA�v�A�O�A� �A��\A�n�A���A��-A�G�A�9XA��A�A�A�\)A���A� �A�x�A���A��A���A��hA�XA�7LA���A�A�A��RA�z�A�r�A�
=A�=qA��;A�VA��uA�ĜA��^A��#A�x�A��\A�;dA�/A}K�Az$�Aw�Av�As�#ArM�Ap��Am��Ak��AkVAh��Ag�hAf�DAdAbE�A`9XA^��A[��A[+AZ�AWhsAU�wAS�TAR�/ARM�AP�AOp�AN�yANM�AM\)ALffAK%AI�AF��AD^5AB�A@I�A>�A=��A<��A;�^A:�A9��A8�A8�A6n�A6$�A5�A5�7A4�9A3�-A2(�A17LA/"�A-��A+dZA+&�A*�`A)VA(1'A&{A%+A$��A#��A"=qA!�A!%A jA7LA��A�yA5?A�mA��A�7A7LA+AhsA�FAK�AbNAdZA�+A"�AbNAx�Ar�A1'A �AJA�AA	�wA	`BA	"�A	AĜA^5A�A��A5?AȴA�#A��A\)A��A��A �A�^AVA��A ĜA -@�-@�1'@�z�@��;@��
@�b@�%@�hs@�%@�r�@���@��@���@�b@�^@�ȴ@�7@��@�@�S�@�
=@�^5@�X@�O�@�X@�G�@睲@��@��T@���@�@�Ĝ@�Z@�t�@�hs@�Q�@�b@��
@�o@ݲ-@�9X@ە�@�;d@ڸR@ى7@�M�@��@���@���@���@�^5@���@�x�@ԃ@�v�@�@ͺ^@�"�@��m@���@�"�@�o@Ͳ-@��#@��T@�G�@�9X@˾w@���@�ȴ@ʟ�@ǅ@ǝ�@�dZ@Ǖ�@ƸR@�v�@�@ź^@�  @�X@��T@���@�r�@���@�9X@���@��y@��@���@��@� �@�(�@�@�V@��j@�@��@���@��@�b@�
=@���@���@�M�@�b@�j@��H@�+@���@�o@�^5@��R@�o@��F@�C�@�ȴ@���@��@���@��+@�E�@�@��9@��@��D@�bN@���@��7@�hs@��@��^@�M�@�S�@�;d@��H@�@��h@�/@��u@���@��y@���@���@���@��-@��#@���@��7@��h@�G�@���@��j@�z�@�1@�dZ@�
=@��@�^5@��T@��/@��
@���@��P@�t�@���@�E�@���@�O�@���@�1'@��@�
=@�ff@��T@��-@��@�I�@�Q�@���@���@�j@�1@�t�@���@��T@���@�~�@�-@�X@�Ĝ@��@��/@��@��@�1'@��@���@�t�@�"�@�n�@�@���@���@���@���@��h@�7L@�%@��j@�Q�@�9X@�(�@� �@�b@���@�V@�5?@�{@�5?@��@���@�x�@��`@�1@�o@�A�@���@��@��u@��@���@��@�dZ@�@�ȴ@���@�~�@��+@�^5@�&�@���@�7L@�V@���@���@��D@��u@��u@��@��w@�l�@�+@�"�@�ȴ@���@��!@��@���@��\@��@�G�@�&�@��j@��/@��j@��D@�r�@�bN@�(�@�1@��@��w@�t�@�;d@��+@�5?@�J@�{@���@��-@�&�@�%@�%@��/@��9@��u@�Z@��;@��
@��
@���@��@���@��+@�v�@�M�@�=q@�5?@�-@��T@��h@��@�p�@�O�@�7L@��`@��D@��@���@�t�@�dZ@�+@��!@�ff@�M�@�^5@�v�@�5?@�-@��7@�hs@�G�@�?}@�&�@�&�@��@���@��`@��j@��@�j@�I�@�(�@�@\)@~�y@~��@~V@}��@|�@|�@|j@|I�@|Z@|�@|�D@|z�@|I�@|�@{��@{C�@{o@z^5@y7L@wK�@v��@v�+@v�+@vE�@v{@u��@u`B@u?}@u`B@uV@t��@t�j@t�j@t�j@tj@sƨ@r��@q��@qX@q��@q%@o��@o
=@o;d@o\)@o
=@n��@nv�@n$�@m�@l��@kC�@j=q@i�@i��@h��@h�9@hbN@h1'@g;d@f�y@f�+@f@e�@e?}@d�@c�m@cS�@c"�@b�@a�#@aX@a&�@a�@`Ĝ@`Q�@` �@`  @_�@_;d@^��@^��@^5?@^@]��@]@]��@]`B@\�@\j@[t�@Z��@Z~�@ZM�@Y��@Y�@XĜ@X��@Xr�@X1'@W�;@W�P@WK�@V��@V�R@Vv�@U��@U�-@U`B@T��@Tz�@T(�@T�@T1@S�m@St�@So@R�@R�!@R~�@Rn�@RM�@R^5@R=q@RJ@Q��@Q�@Pr�@P  @O�@O��@O�@O�P@O|�@O\)@Nȴ@Nff@N5?@M��@Mp�@L��@LI�@K�
@K��@K�@Kt�@KdZ@Ko@Jn�@J�@I��@I�7@I�@I�@H��@H�9@HbN@HA�@H �@Hb@G��@G\)@F��@FV@F{@E�T@E@E�@E/@EV@D��@Dj@D9X@D(�@C�
@C"�@B�@B�@B�!@B�\@Bn�@BM�@BM�@B=q@A��@Ahs@@��@@r�@@r�@@bN@?�;@?�P@?;d@>�@>��@>E�@>@=�@=�-@=/@<�@<��@<�@<z�@<9X@;��@;��@;�@;S�@;"�@:�@:�\@:^5@9��@9�7@97L@8�`@8Ĝ@8�u@8A�@8 �@8 �@7�w@7\)@7�@6ȴ@6V@5��@5`B@5?}@5V@4�/@49X@4�@3��@3��@41@41@41@3��@3��@333@1�@0��@0 �@/�w@/l�@/;d@/
=@.�R@.��@.V@.@-�@-V@,��@,�j@,1@+dZ@+@*�H@*��@*�\@*J@)��@)��@)hs@)hs@)X@)7L@(�`@(��@(�@(Q�@(  @'�@'|�@&��@&E�@%�@%?}@%�@$�@$I�@#��@#"�@"��@"-@!��@!X@ ��@ ��@ Q�@ 1'@   @�P@;d@�y@ff@@�-@?}@��@�/@�D@(�@�@1@1@��@�m@ƨ@�F@��@�@dZ@C�@@�!@�\@~�@~�@J@X@��@��@Ĝ@�u@r�@r�@Q�@Q�@1'@ �@ �@ �@  @�;@�P@|�@\)@��@ȴ@�R@��@�+@E�@{@�T@�-@O�@/@�@V@z�@1@�F@�@dZ@o@��@�\@=q@J@��@��@x�@&�@�`@��@��@�@�@�@r�@Q�@A�@1'@  @��@l�@\)@;d@+@�@�+@ff@V@5?@{@�@�T@�-@�h@�h@�@�@V@V@V@�@�/@�j@z�@9X@1@��@�m@��@"�@"�@"�@"�@o@
��@
��@
�\@
^5@
M�@
=q@
=q@
J@	��@	X@	�@��@Ĝ@�9@�@Q�@1'@ �@ �@b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�sB�fB�fB�fB�fB�fB�fB�`B�fB�mB�yB�B�B�B�BBoB��B�B�yB�B�B�B��BPB�B'�B33B=qBA�BK�BR�B]/BjBx�B�FB)�B5?B6FB�BB�B%�BuB
=BB��B��B  BJB%B+BB��B��B�B�B��B��B�B�BɺB��B~�B<jB'�BPB�yB�ZB�BĜB�!B��By�BL�B;dB�BVB
�ZB
B
�B
��B
�B
l�B
R�B
>wB
2-B
�B
{B
	7B	��B	�NB	�/B	ɺB	�RB	�!B	��B	��B	�JB	� B	p�B	hsB	ffB	VB	P�B	G�B	C�B	@�B	<jB	49B	0!B	-B	)�B	$�B	 �B	�B	hB	%B	B��B��B��B��B��B�B�B�B�fB�HB�;B�5B�/B�#B�B��B��BɺBƨB��B�qB�dB�?B�B��B��B��B��B��B��B��B��B�uB�{B�bB�VB�JB�JB�DB�7B�%B�B{�By�Bw�Bs�Bq�Bk�BhsBiyBiyBiyBiyBiyBhsBhsBjBiyBhsBgmBgmBffBffBe`BgmBffBffBhsBr�B{�By�Bv�By�B�bB��B�PB�oB�JB�B�B�=B��B��B�^BĜBɺBɺBɺBɺBŢB��B�^B�LB�jB�wB��BɺBɺB��B��B��B��B�B�/B�)B�/B�5B�;B�;B�BB�sB�B�B�B�B�B�B�B��B��B��B�B��B	B	B	%B		7B	bB	bB	�B	hB	�B	{B	\B	�B	!�B	$�B	&�B	,B	)�B	33B	<jB	@�B	C�B	H�B	N�B	S�B	R�B	K�B	W
B	]/B	e`B	jB	jB	jB	k�B	hsB	aHB	XB	W
B	ZB	cTB	cTB	e`B	bNB	dZB	gmB	cTB	bNB	dZB	p�B	t�B	q�B	cTB	m�B	r�B	r�B	t�B	v�B	v�B	z�B	{�B	u�B	~�B	y�B	}�B	�%B	�B	�B	�7B	�JB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�3B	�3B	�3B	�FB	�^B	�XB	�RB	�9B	�?B	�LB	�wB	�qB	�qB	�}B	B	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ɺB	��B	��B	��B	��B	ɺB	ȴB	ȴB	ȴB	ǮB	ƨB	ŢB	ŢB	ĜB	ÖB	ŢB	��B	��B	��B	ȴB	ǮB	ƨB	ĜB	ĜB	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�BB	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�fB	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
+B
+B
+B
%B
B
B
B
	7B
DB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
PB
JB
PB
PB
JB
DB
DB
JB
JB
JB
JB
JB
JB
PB
VB
VB
\B
VB
VB
VB
VB
VB
VB
\B
bB
hB
oB
uB
uB
uB
oB
hB
hB
oB
oB
oB
hB
oB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
!�B
!�B
 �B
�B
�B
�B
�B
�B
�B
#�B
$�B
%�B
(�B
(�B
'�B
'�B
'�B
(�B
)�B
)�B
'�B
%�B
%�B
(�B
(�B
'�B
'�B
)�B
,B
,B
,B
,B
,B
,B
,B
+B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
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
0!B
0!B
0!B
1'B
2-B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
9XB
9XB
:^B
:^B
;dB
;dB
<jB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
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
D�B
D�B
D�B
F�B
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
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
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
T�B
T�B
T�B
VB
VB
W
B
XB
XB
YB
XB
W
B
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
YB
YB
YB
YB
ZB
[#B
\)B
\)B
\)B
\)B
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
^5B
^5B
_;B
^5B
^5B
_;B
_;B
`BB
_;B
_;B
`BB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
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
iyB
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
p�B
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
u�B
u�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
z�B
z�B
z�B
{�B
{�B
{�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BB�(B�B��B�*B�>B�B�B��B�aB��B�B�B�B�B�B��B�B�B�B��B�B��B�iB�BEB�B��B�B�B��B��B��B�B�B�B)*B4�B>BBB�BL�BS�B^Bj�By�B��B+QB8�B<jB"NBYBsB)B�BpB�B��B�fB�B<B�B	RB�B�^B��B��B��B�VB�PB��B��BΥB��B��BAUB-�B�B�WB�mB�BȀB�B��B�BQB?�B �B�B
�yB
ňB
�|B
��B
��B
p;B
U�B
AB
4�B
!�B
�B
�B	�	B	��B	ߊB	��B	�DB	�3B	�HB	�1B	��B	�B	r-B	j�B	iyB	XyB	SB	IB	D�B	B[B	>(B	5?B	1AB	.}B	+�B	'B	#�B	�B	aB	�B	B��B��B�`B�xB�>B�%B�B��B�XB��B��B�;BޞB��B�7B��BуB��B�RB�;B��B��B��B��B�_B��B�nB�vB��B��B��B��B�mB��B�hB��B��B�B��B�B��B�-B}B{dByXBu?Bs�Bl�Bi�Bj�BjBi�Bi�Bj0Bi�BjBkBjBh�Bh
BhXBgmBg�BgRBiDBg�BgBiBr�B|�Bz�Bw2ByXB�hB��B��B��B�"B�+B��B�rB��B��B��B�9B�rBʦBʦB��B�B�[B�PB�lB��B�HB��B�=B�XB�pB�(B�@B՛B�qB�5B�xBݘB��B��B��B�bB��B�QB��B�B�WB�B��B�AB�?B�ZB�tB��B��B	?B	�B	�B		�B	 B	B	�B	@B	�B	B	�B	_B	"B	%�B	'mB	,�B	*KB	3�B	="B	AoB	D3B	I�B	OBB	T�B	T�B	LB	WYB	]dB	fB	j�B	kB	k6B	m)B	j�B	cTB	YB	WYB	ZB	c�B	c�B	e�B	b�B	e,B	hXB	dB	bNB	c�B	qB	vB	r�B	b�B	m�B	r�B	r�B	u�B	wLB	w2B	{B	}<B	v+B	�B	y�B	~(B	��B	��B	�B	�B	�JB	��B	�B	��B	��B	� B	�8B	�RB	��B	��B	�tB	��B	�fB	��B	��B	��B	��B	��B	��B	��B	�hB	��B	��B	��B	��B	��B	�$B	��B	��B	�fB	��B	��B	��B	��B	�B	��B	�B	�	B	�B	�)B	�dB	�PB	�VB	�.B	�hB	҉B	��B	ϑB	�)B	��B	�#B	�dB	�JB	�DB	�^B	�XB	�lB	�RB	�7B	�KB	�EB	�B	�?B	�B	��B	żB	�B	�0B	�^B	�RB	ȀB	�EB	��B	ĜB	�VB	�pB	�PB	�B	��B	��B	�TB	�uB	�NB	�NB	�NB	�hB	ЗB	�HB	�(B	�(B	�B	�B	�BB	�TB	�aB	�mB	׍B	�EB	�_B	�EB	�B	��B	��B	�B	��B	��B	��B	�B	�!B	�/B	�"B	��B	�B	�B	��B	��B	��B	�3B	�%B	�+B	�+B	�B	�B	��B	�B	�FB	��B	�B	��B	�	B	�0B	�0B	�B	�B	�B	�VB	�qB	�<B	�VB	�BB
 iB
 OB
 B
GB
zB
zB
�B
�B
gB
gB
SB
	lB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
�B
B
)B
�B
B
B
�B
�B
�B
!�B
"B
!�B
!�B
!-B
"B
"4B
"NB
"hB
!�B
!B
B
�B
�B
�B
 'B
$&B
%,B
%�B
)DB
)DB
($B
(
B
(>B
)_B
*eB
*B
(sB
&2B
&B
)yB
)�B
(XB
($B
*B
,WB
,WB
,=B
,WB
,qB
,�B
,�B
+�B
*eB
*eB
*KB
*0B
*KB
*eB
+kB
+QB
,qB
-wB
-wB
.cB
.cB
/�B
/iB
/iB
/�B
0oB
0�B
0oB
0oB
0oB
0�B
0oB
0oB
0oB
1�B
2aB
1�B
2|B
2aB
2aB
2aB
2|B
2|B
2�B
3�B
3�B
4�B
3�B
3�B
3�B
4�B
4�B
4�B
4�B
4�B
5tB
6�B
6zB
6�B
7�B
7�B
7�B
7�B
7�B
8�B
9�B
9�B
:xB
:�B
:�B
:�B
9�B
9�B
:�B
:�B
;�B
;�B
<�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
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
D�B
D�B
EB
GB
E�B
FB
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
HB
IB
IB
H�B
H�B
IB
J	B
I�B
I�B
I�B
J#B
J#B
J#B
KB
J�B
KB
K)B
LB
K�B
LB
MB
MB
MB
MB
MB
N<B
N"B
N"B
N"B
NB
NB
N"B
O(B
O(B
OB
P.B
PB
PB
P.B
PHB
QNB
Q4B
QB
Q4B
Q4B
R:B
R B
R:B
R:B
S&B
S@B
S[B
S@B
S@B
TFB
TFB
T,B
T,B
TFB
U2B
U2B
U2B
VB
VB
W?B
XEB
XyB
YB
X�B
W�B
V�B
W?B
WYB
W?B
W?B
W?B
WYB
X_B
XyB
XyB
YeB
YeB
YeB
Y�B
Z�B
[qB
\xB
\xB
\xB
\xB
]~B
^jB
^jB
^OB
^jB
^�B
^�B
^�B
^jB
^�B
^�B
^jB
_pB
^�B
^�B
_�B
_�B
`vB
_�B
_�B
`�B
a�B
a�B
a�B
b�B
b�B
c�B
c�B
c�B
d�B
d�B
d�B
e�B
e�B
e�B
f�B
f�B
f�B
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
i�B
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
m�B
m�B
m�B
n�B
n�B
n�B
o B
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
p�B
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
r�B
r�B
r�B
r�B
r�B
r�B
s�B
tB
s�B
tB
tB
s�B
t�B
t�B
t�B
t�B
uB
t�B
u�B
u�B
v�B
wB
xB
xB
xB
xB
y$B
x�B
y$B
y	B
z*B
zB
{B
z�B
{B
{0B
|6B
|B
|B
{B
z�B
{0B
|6B
|6B
|6B
{0B
{B
z�B
{0B
{0B
{0B
|B
|B
|B
}"B
}"111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.34(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201607060037452016070600374520160706003745201806221258432018062212584320180622125843201804050657192018040506571920180405065719  JA  ARFMdecpA19c                                                                20160702093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160702003518  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160702003519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160702003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160702003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160702003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160702003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160702003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160702003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160702003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20160702011744                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160702153719  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20160705153745  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160705153745  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404215719  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622035843  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201515                      G�O�G�O�G�O�                