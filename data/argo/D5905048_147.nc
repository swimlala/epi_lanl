CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-08-08T00:35:14Z creation;2017-08-08T00:35:17Z conversion to V3.1;2019-12-19T08:00:36Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20170808003514  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_147                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @����� 1   @��s�� @4�e��O�d�/�V��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@z�H@�=q@�=qA�A=�A]�A}�A��\A��\A��\A��\AΏ\A�\)A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ�RCS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D nD �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK��DLz�DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�}qD��qD��=D�:=D�z=D��=D��
D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�7
D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�w
D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�w
D��=D��qD�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�:=D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�:=D�z=D׺=D��=D�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A�A�  A�  A�  A�  A�  A�A��
A�%AۮA�?}A�bNA���Aذ!A׏\AՅA�=qA�t�A���AΡ�A�(�A���A�/A��A��A�AōPAĸRAüjA���A���A�ȴA���A��jA��\A�I�A���A�1A�7LA��HA���A�A�9XA�t�A�JA�XA��A���A���A��PA�C�A��HA���A�E�A���A�K�A��A�oA�\)A��#A�A�JA��A�x�A�ĜA��
A��+A��TA��A��mA��^A��A�bA�I�A���A��`A�dZA�bA��`A�ȴA�ffA�n�A�z�A�S�A���A���A�/A�ƨA�x�A�E�A���A���A�5?A�r�A�t�A��/A��/A��jA�G�A|��AyXAwp�Au�TAsO�Aq7LAnAiK�Ag�7Af(�Ad�Ac�Abr�Aa;dA_XA]�#A[��A[��AZ �AX��AX  AW�-AW%AV  AU%ATjAS�AR~�AP(�AO�mAO�AN�uALM�AJ�9AH�AG%AE��AEhsADr�ABbNAA��AA7LA?�A>z�A=`BA;��A:�yA:M�A7�hA5�PA3��A1S�A/XA/
=A.��A.�!A-�A,��A,(�A+��A+�A*M�A)�A(z�A'�A'�A&��A&�\A&{A%�
A%��A%�wA%�7A%;dA$E�A"��A!
=A ��A {AS�A�`A��AoA�^A�A{AoA��A`BA��A�A�DAO�AȴA=qA�A`BA�+A�A��A/AK�A^5A��A%A
I�A	t�A��A33A�!Az�AE�A��A{Al�A%@��@��9@���@�1'@�@��;@�I�@��m@���@�R@�!@��^@��@���@홚@�?}@�A�@�@�9@畁@�v�@��T@��@�D@�D@�z�@㝲@��y@���@��`@���@ڸR@�=q@١�@��@�Ĝ@؋D@���@�~�@�&�@ԓu@� �@��
@��@Ѓ@�K�@��y@�~�@�M�@�@�S�@Ɂ@��@���@�  @���@Ɵ�@��@ċD@Ĭ@öF@�+@�$�@�A�@�~�@���@�|�@�@���@�A�@���@�K�@�o@��!@�5?@�I�@���@��H@�^5@�ff@�n�@�V@�^5@�=q@�@�O�@��j@� �@��
@��F@�t�@�33@�@���@�$�@���@�O�@��@�%@���@��`@��/@��/@���@���@���@�A�@� �@�1'@�b@��;@�|�@���@���@�^5@�{@���@��h@�?}@�Ĝ@�bN@�(�@���@���@���@�dZ@�o@�v�@�=q@�J@��T@�@�`B@�X@�O�@�G�@�7L@�/@��@��/@���@�bN@�(�@��;@���@�K�@��@��H@���@��\@�~�@�ff@�{@��^@��@�O�@�&�@��@��j@���@��u@��;@���@�t�@�33@��@�
=@��y@��\@�-@���@�O�@���@��u@�A�@� �@�  @��m@��;@��
@���@�ƨ@�ƨ@��w@��@�K�@��@���@��+@�^5@�E�@��#@�@��-@���@���@��@�7L@��/@��@�z�@�r�@��D@���@��@�j@�bN@�I�@� �@���@�33@��y@��+@�5?@���@���@��^@��7@�G�@�/@��/@��@�z�@�A�@��@��
@�C�@��@�ȴ@�^5@�$�@��@��-@���@��h@�hs@�G�@��@��@���@��j@���@�j@�A�@���@�dZ@�o@���@���@�ff@�{@�@��h@�hs@�/@���@��D@�bN@�I�@� �@��m@��w@��P@�l�@���@���@�~�@�{@���@�@��h@�`B@�7L@���@��j@��u@�1'@���@��@��P@�|�@�\)@�"�@�"�@�
=@��@���@��\@�E�@���@��T@���@�@���@�X@��@�%@���@��@�Ĝ@�r�@�1'@�1@��@��;@��w@�S�@�
=@�ȴ@���@��R@��+@�^5@�$�@���@���@�`B@���@��j@���@�I�@�  @K�@~��@}p�@|Z@{t�@{o@z�@z�\@zM�@zJ@y�7@y&�@x��@x1'@w��@w�P@w\)@w;d@w�@v�y@v�y@vȴ@v��@v�+@vv�@vE�@v$�@v{@u�@u�h@u�@t�/@tZ@t1@s�@r��@r�\@rn�@q�@q��@qx�@p��@p��@pQ�@o�w@o�P@o+@nȴ@nv�@m�T@m`B@m�@mV@l�@l�@l�D@l�D@lz�@l9X@k��@kƨ@kt�@k33@j��@j~�@i��@i��@h��@h  @g��@gK�@f�R@f$�@e�T@e�-@e?}@d��@dZ@c��@c��@cC�@b�!@a�#@aG�@a�@`Ĝ@` �@_��@_�P@_l�@_l�@_\)@_K�@_+@^ȴ@^$�@]�-@]p�@]O�@\��@\��@\j@\(�@[�
@[dZ@Z�@Yhs@Y7L@Y%@X��@X�@W�@WK�@W�@Vv�@U�@U�-@U�h@Up�@T�@T9X@S�F@SC�@R��@R~�@RM�@Q��@Q&�@P�`@P�u@O�;@O�P@O+@Nȴ@Nv�@M@L��@L9X@K�m@K�@Ko@J~�@JJ@I��@I�7@IG�@H�`@H�@HQ�@G��@G\)@G+@G
=@F�y@F�R@F�+@FE�@E��@D��@Dj@DI�@D�@C��@C��@C�m@Cƨ@C�F@C��@Ct�@C"�@Co@B��@B~�@Bn�@B=q@A��@A�^@A��@AG�@@��@@ �@?�;@?��@?K�@>��@>��@>��@>��@>�+@>ff@>5?@=�@=�-@=�h@=p�@=`B@=?}@<�/@<��@<�D@<Z@<I�@<�@;�m@;��@;S�@:�!@:^5@:-@9�@9��@9x�@9�@8��@8�`@8��@8A�@7�;@7�w@7+@6�R@6��@6v�@6V@5�@5p�@5?}@5/@4�/@4�j@4�@4�D@4z�@4z�@4j@4Z@49X@4�@4�@4�@3��@3@2��@2n�@2^5@2-@2J@2J@1��@1�@1�@1�@1�#@1��@1G�@0��@0�@/�;@/��@/|�@/\)@/;d@.��@.�+@.ff@.ff@.E�@-�h@,��@,I�@+��@+t�@+S�@+o@*��@*�!@*�\@*~�@*^5@*^5@*^5@*M�@)��@)�#@)��@)X@)�@(�9@(bN@(  @'�w@'l�@'+@&��@&ȴ@&�+@&ff@%�T@%�h@%�@%p�@%`B@%`B@%O�@%?}@%�@%V@$��@$�@$�/@$��@$�j@$(�@#ƨ@#t�@#t�@#dZ@#S�@#"�@#"�@#"�@#@"��@"~�@"M�@"M�@"=q@"-@"�@"J@"J@!�@!�@!�@!�#@!��@!��@!x�@!hs@!hs@!X@!7L@!%@ Ĝ@ �9@ bN@ 1'@  �@   @�w@��@K�@�y@�@�@�@�@�@�R@��@�+@ff@5?@{@@�@�T@�-@�@`B@?}@/@�@��@��@j@Z@I�@9X@�@��@��@�F@��@��@S�@33@��@~�@-@��@X@G�@�@��@Ĝ@��@r�@b@��@��@�P@|�@\)@K�@�@
=@�@�R@��@�+@ff@{@�-@�h@�@p�@`B@V@��@�D@j@9X@�@1@��@�
@�F@��@�@C�@�H@�H@��@�!@~�@=q@�#@��@G�@�@%@��@��@��@��@�9@A�@�@��@�@��@l�@K�@+@�@�@��@�@�R@v�@ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A�A�  A�  A�  A�  A�  A�A��
A�%AۮA�?}A�bNA���Aذ!A׏\AՅA�=qA�t�A���AΡ�A�(�A���A�/A��A��A�AōPAĸRAüjA���A���A�ȴA���A��jA��\A�I�A���A�1A�7LA��HA���A�A�9XA�t�A�JA�XA��A���A���A��PA�C�A��HA���A�E�A���A�K�A��A�oA�\)A��#A�A�JA��A�x�A�ĜA��
A��+A��TA��A��mA��^A��A�bA�I�A���A��`A�dZA�bA��`A�ȴA�ffA�n�A�z�A�S�A���A���A�/A�ƨA�x�A�E�A���A���A�5?A�r�A�t�A��/A��/A��jA�G�A|��AyXAwp�Au�TAsO�Aq7LAnAiK�Ag�7Af(�Ad�Ac�Abr�Aa;dA_XA]�#A[��A[��AZ �AX��AX  AW�-AW%AV  AU%ATjAS�AR~�AP(�AO�mAO�AN�uALM�AJ�9AH�AG%AE��AEhsADr�ABbNAA��AA7LA?�A>z�A=`BA;��A:�yA:M�A7�hA5�PA3��A1S�A/XA/
=A.��A.�!A-�A,��A,(�A+��A+�A*M�A)�A(z�A'�A'�A&��A&�\A&{A%�
A%��A%�wA%�7A%;dA$E�A"��A!
=A ��A {AS�A�`A��AoA�^A�A{AoA��A`BA��A�A�DAO�AȴA=qA�A`BA�+A�A��A/AK�A^5A��A%A
I�A	t�A��A33A�!Az�AE�A��A{Al�A%@��@��9@���@�1'@�@��;@�I�@��m@���@�R@�!@��^@��@���@홚@�?}@�A�@�@�9@畁@�v�@��T@��@�D@�D@�z�@㝲@��y@���@��`@���@ڸR@�=q@١�@��@�Ĝ@؋D@���@�~�@�&�@ԓu@� �@��
@��@Ѓ@�K�@��y@�~�@�M�@�@�S�@Ɂ@��@���@�  @���@Ɵ�@��@ċD@Ĭ@öF@�+@�$�@�A�@�~�@���@�|�@�@���@�A�@���@�K�@�o@��!@�5?@�I�@���@��H@�^5@�ff@�n�@�V@�^5@�=q@�@�O�@��j@� �@��
@��F@�t�@�33@�@���@�$�@���@�O�@��@�%@���@��`@��/@��/@���@���@���@�A�@� �@�1'@�b@��;@�|�@���@���@�^5@�{@���@��h@�?}@�Ĝ@�bN@�(�@���@���@���@�dZ@�o@�v�@�=q@�J@��T@�@�`B@�X@�O�@�G�@�7L@�/@��@��/@���@�bN@�(�@��;@���@�K�@��@��H@���@��\@�~�@�ff@�{@��^@��@�O�@�&�@��@��j@���@��u@��;@���@�t�@�33@��@�
=@��y@��\@�-@���@�O�@���@��u@�A�@� �@�  @��m@��;@��
@���@�ƨ@�ƨ@��w@��@�K�@��@���@��+@�^5@�E�@��#@�@��-@���@���@��@�7L@��/@��@�z�@�r�@��D@���@��@�j@�bN@�I�@� �@���@�33@��y@��+@�5?@���@���@��^@��7@�G�@�/@��/@��@�z�@�A�@��@��
@�C�@��@�ȴ@�^5@�$�@��@��-@���@��h@�hs@�G�@��@��@���@��j@���@�j@�A�@���@�dZ@�o@���@���@�ff@�{@�@��h@�hs@�/@���@��D@�bN@�I�@� �@��m@��w@��P@�l�@���@���@�~�@�{@���@�@��h@�`B@�7L@���@��j@��u@�1'@���@��@��P@�|�@�\)@�"�@�"�@�
=@��@���@��\@�E�@���@��T@���@�@���@�X@��@�%@���@��@�Ĝ@�r�@�1'@�1@��@��;@��w@�S�@�
=@�ȴ@���@��R@��+@�^5@�$�@���@���@�`B@���@��j@���@�I�@�  @K�@~��@}p�@|Z@{t�@{o@z�@z�\@zM�@zJ@y�7@y&�@x��@x1'@w��@w�P@w\)@w;d@w�@v�y@v�y@vȴ@v��@v�+@vv�@vE�@v$�@v{@u�@u�h@u�@t�/@tZ@t1@s�@r��@r�\@rn�@q�@q��@qx�@p��@p��@pQ�@o�w@o�P@o+@nȴ@nv�@m�T@m`B@m�@mV@l�@l�@l�D@l�D@lz�@l9X@k��@kƨ@kt�@k33@j��@j~�@i��@i��@h��@h  @g��@gK�@f�R@f$�@e�T@e�-@e?}@d��@dZ@c��@c��@cC�@b�!@a�#@aG�@a�@`Ĝ@` �@_��@_�P@_l�@_l�@_\)@_K�@_+@^ȴ@^$�@]�-@]p�@]O�@\��@\��@\j@\(�@[�
@[dZ@Z�@Yhs@Y7L@Y%@X��@X�@W�@WK�@W�@Vv�@U�@U�-@U�h@Up�@T�@T9X@S�F@SC�@R��@R~�@RM�@Q��@Q&�@P�`@P�u@O�;@O�P@O+@Nȴ@Nv�@M@L��@L9X@K�m@K�@Ko@J~�@JJ@I��@I�7@IG�@H�`@H�@HQ�@G��@G\)@G+@G
=@F�y@F�R@F�+@FE�@E��@D��@Dj@DI�@D�@C��@C��@C�m@Cƨ@C�F@C��@Ct�@C"�@Co@B��@B~�@Bn�@B=q@A��@A�^@A��@AG�@@��@@ �@?�;@?��@?K�@>��@>��@>��@>��@>�+@>ff@>5?@=�@=�-@=�h@=p�@=`B@=?}@<�/@<��@<�D@<Z@<I�@<�@;�m@;��@;S�@:�!@:^5@:-@9�@9��@9x�@9�@8��@8�`@8��@8A�@7�;@7�w@7+@6�R@6��@6v�@6V@5�@5p�@5?}@5/@4�/@4�j@4�@4�D@4z�@4z�@4j@4Z@49X@4�@4�@4�@3��@3@2��@2n�@2^5@2-@2J@2J@1��@1�@1�@1�@1�#@1��@1G�@0��@0�@/�;@/��@/|�@/\)@/;d@.��@.�+@.ff@.ff@.E�@-�h@,��@,I�@+��@+t�@+S�@+o@*��@*�!@*�\@*~�@*^5@*^5@*^5@*M�@)��@)�#@)��@)X@)�@(�9@(bN@(  @'�w@'l�@'+@&��@&ȴ@&�+@&ff@%�T@%�h@%�@%p�@%`B@%`B@%O�@%?}@%�@%V@$��@$�@$�/@$��@$�j@$(�@#ƨ@#t�@#t�@#dZ@#S�@#"�@#"�@#"�@#@"��@"~�@"M�@"M�@"=q@"-@"�@"J@"J@!�@!�@!�@!�#@!��@!��@!x�@!hs@!hs@!X@!7L@!%@ Ĝ@ �9@ bN@ 1'@  �@   @�w@��@K�@�y@�@�@�@�@�@�R@��@�+@ff@5?@{@@�@�T@�-@�@`B@?}@/@�@��@��@j@Z@I�@9X@�@��@��@�F@��@��@S�@33@��@~�@-@��@X@G�@�@��@Ĝ@��@r�@b@��@��@�P@|�@\)@K�@�@
=@�@�R@��@�+@ff@{@�-@�h@�@p�@`B@V@��@�D@j@9X@�@1@��@�
@�F@��@�@C�@�H@�H@��@�!@~�@=q@�#@��@G�@�@%@��@��@��@��@�9@A�@�@��@�@��@l�@K�@+@�@�@��@�@�R@v�@ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BbB\BbBbB\B\BVBDB$�B��B+B\B(�B(�B0!B9XB/BD�BZBcTBcTB\)BS�BM�BXBW
B:^B=qB1'B&�B�B\B5?BB�B^5Br�B�B� B�B�bB�\B�1B}�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�Bt�B|�B|�Bw�Bu�Bp�BhsBe`BhsBO�BB�B+BuBB�fB�dB�bB�%B�Bx�Br�Bk�B}�B�1Bs�B^5BZBT�BG�B6FB{B
�;B
��B
�9B
��B
�hB
�=B
�=B
s�B
O�B
F�B
<jB
"�B
!�B
�B
�B
DB	��B	�;B	ȴB	B	�qB	�-B	�B	��B	��B	�oB	�B	y�B	z�B	r�B	iyB	jB	ffB	bNB	W
B	Q�B	N�B	I�B	?}B	1'B	7LB	33B	'�B	�B	oB	+B��B��B	B��B�B�B�B�yB�BB�/B�
B��B��B�}B�?B�!B��B��B��B��B��B��B��B��B��B��B�{B�VB�bB�\B�JB�VB�\B�JB�PB�VB�PB�7B�%B~�Bu�Bt�By�Bw�Br�Br�BjBk�Be`BdZBdZB`BB`BBXBT�BW
BW
BN�BQ�BQ�BP�BQ�BK�BI�BE�BJ�BYB`BBffBgmBhsBhsBiyBhsBjBhsBffB^5BaHBjBcTB\)BbNB^5Be`BdZB]/BffBq�Bn�BcTBo�Bu�B|�B}�B~�B}�By�Bs�Bw�By�Bz�B{�B{�Bz�B|�B�B�B�B� Bz�Br�Bl�B{�B~�B� B�B�B}�B{�B�B�B�+B�DB�JB�B�7B�VB�PB�bB�bB�\B�{B��B��B��B��B��B��B��B��B�B�B�B�B��B�!B�B�^B�wB��BɺB��B��B��B�B�B�;B�`B�B�B	  B	B	+B		7B	JB	JB	PB	PB	\B	bB	\B	\B	oB	oB	�B	�B	�B	$�B	(�B	)�B	,B	,B	,B	,B	,B	,B	,B	2-B	6FB	6FB	5?B	5?B	6FB	=qB	=qB	B�B	E�B	G�B	H�B	J�B	N�B	Q�B	R�B	S�B	S�B	VB	W
B	YB	`BB	bNB	dZB	e`B	ffB	m�B	o�B	o�B	o�B	o�B	o�B	o�B	q�B	u�B	v�B	w�B	y�B	|�B	� B	�B	�B	�1B	�7B	�7B	�7B	�PB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�?B	�RB	�^B	�qB	�}B	��B	��B	��B	B	B	B	B	B	��B	B	ÖB	ƨB	ǮB	ǮB	ɺB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�5B	�BB	�HB	�NB	�NB	�NB	�`B	�`B	�`B	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
%B
1B
	7B
	7B
	7B

=B
DB
DB
JB
JB
JB
PB
VB
VB
VB
VB
PB
VB
\B
bB
\B
\B
VB
\B
bB
hB
hB
hB
bB
hB
oB
uB
uB
uB
uB
oB
oB
uB
uB
uB
{B
�B
{B
�B
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
'�B
(�B
)�B
+B
)�B
+B
,B
-B
,B
,B
-B
.B
/B
.B
/B
/B
0!B
1'B
1'B
1'B
2-B
49B
49B
5?B
49B
49B
49B
33B
33B
49B
5?B
6FB
5?B
6FB
5?B
6FB
5?B
5?B
49B
6FB
9XB
9XB
9XB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
9XB
:^B
:^B
;dB
;dB
;dB
<jB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
=qB
=qB
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
B�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
C�B
C�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
G�B
H�B
H�B
G�B
H�B
H�B
H�B
H�B
I�B
H�B
H�B
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
K�B
K�B
K�B
K�B
K�B
L�B
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
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
N�B
O�B
P�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
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
S�B
S�B
T�B
S�B
S�B
Q�B
T�B
T�B
VB
VB
VB
W
B
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
XB
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
ZB
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
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
_;B
_;B
`BB
`BB
`BB
`BB
aHB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
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
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
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
iyB
iyB
iyB
iyB
jB
iyB
iyB
iyB
jB
jB
jB
k�B
jB
k�B
k�B
jB
k�B
k�B
jB
k�B
k�B
k�B
k�B
k�B
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
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
q�B
r�B
s�B
r�B
s�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
u�B
t�B
t�B
t�B
t�B
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
v�B
v�B
w�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BbB\BbB}BvB�B�B�B)�B��BfB4B*KB+�B3MB=�B4BHKB]dBf2Bf�B_;BW?BP�BZ7BYeB=�B?HB3�B)DB�BuB7LBESB`�Bu%B�AB��B��B�4B�bB�#B�UB�B��B�bB��B�B��B��B��B��B�pB��B�B��B��BxB~�B~wBy�Bw�BrGBj�BgmBj�BR�BD�B/ B�BzB�kB��B�B��B��BzBshBl"B~�B�RBvB`�B\�BV�BI�B9XBB
�B
�uB
��B
��B
��B
��B
��B
y>B
S�B
G�B
>wB
'�B
%�B
"4B
 �B
�B	�B	�B	͟B	��B	�cB	��B	�cB	�6B	��B	��B	�+B	|B	{�B	t�B	k6B	kkB	gB	cTB	XyB	S@B	O�B	J�B	A�B	3�B	7�B	4B	)�B	eB	�B		�B	UB�wB	'B�jB�B��B�B�QB�NB��B��B�uB�bB��B�B��B��B��B�LB�fB�ZB��B��B��B��B�B��B��B�NB�HB�jB��B��B�B��B��B��B��B�B��BxBv�Bz�Bx�Bs�Bs�Bl=Bl�Bg8Be�Be�Ba�BabBZBW
BXBX+BP}BR�BR�BQ�BR�BM6BK^BG�BK�BYBa�Bg�Bh�Bi�Bi�Bj�Bj0Bk6BiBg8B`'Bb�Bl=Bf�B^�Bc�B`'Bf�Be�B^�Bf�Br|Bp;Be`Bo�Bv`B|�B~(B}B~�Bz�BuZBx�Bz�B{�B|�B|�B{dB}"B�uB��B��B��B|Bt�Bn�B|PB�B��B�UB�oB~�B}B��B��B��B��B�PB��B�#B��B��B��B�NB�B��B�#B�4B��B��B�ZB��B��B�LB��B��B�!B�}B�eB�[B�;B��B�}B��B�#B�^B�FBՁB��B�WB߾B��B��B��B	 B	GB	_B		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	 'B	%B	)*B	*B	,=B	,=B	,"B	,=B	,=B	,WB	,qB	2aB	6`B	6�B	5�B	5�B	6�B	=�B	=�B	B�B	E�B	G�B	IB	KDB	OBB	R:B	S@B	TFB	TFB	V9B	WYB	YB	`�B	b�B	d�B	e�B	f�B	m�B	o�B	o�B	o�B	o�B	o�B	o�B	q�B	vB	wB	xB	z*B	}<B	�OB	�aB	�mB	�fB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�=B	�B	�B	�>B	�6B	�=B	�WB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	B	��B	��B	��B	ªB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	��B	�B	�"B	��B	�B	�B	�B	�B	�2B	�9B	�?B	�9B	�SB	�gB	ՁB	�_B	�B	�kB	�qB	�]B	�]B	�xB	�xB	�dB	�~B	ބB	��B	�|B	�B	�B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�2B	�$B	�B	�B	�0B	�0B	�B	�6B	�<B	�6B	�BB	�HB	�HB
AB
[B
[B
GB
aB
aB
gB
mB
�B
tB
tB
fB
	lB
	lB
	lB

rB
xB
�B
~B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B

B
�B
�B
�B
�B
�B
�B
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
B
B
�B
B
�B
B
!B
 �B
 �B
!B
!�B
!�B
"B
#B
# B
# B
$B
$&B
%,B
&2B
&2B
&2B
'B
(
B
($B
'B
(
B
'�B
($B
($B
($B
($B
($B
($B
(>B
(>B
(>B
)DB
(XB
)_B
*0B
+B
*KB
+QB
,=B
-CB
,WB
,WB
-]B
.cB
/OB
.cB
/iB
/�B
0oB
1[B
1vB
1vB
2aB
4nB
4nB
5ZB
4nB
4nB
4nB
3�B
3hB
4�B
5tB
6zB
5tB
6zB
5�B
6`B
5�B
5�B
4�B
6�B
9�B
9�B
9�B
8�B
8�B
8�B
9rB
9�B
9�B
:�B
:�B
:�B
9�B
:�B
:�B
;�B
;�B
;B
<�B
;�B
;�B
<�B
<�B
<�B
=�B
=�B
=�B
>�B
=�B
=�B
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
B�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
C�B
C�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
G�B
H�B
H�B
G�B
H�B
H�B
H�B
H�B
I�B
IB
IB
J	B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
K�B
K�B
MB
MB
L�B
L�B
MB
MB
MB
L�B
MB
NB
OB
OB
OB
OB
OB
PB
O�B
PB
O(B
PB
QB
P.B
QB
R B
R B
R B
R:B
R:B
S&B
S&B
S&B
T,B
TB
T,B
TB
TB
TB
S�B
T,B
T,B
UB
T,B
T,B
RoB
U2B
U2B
VB
V9B
V9B
W$B
VB
W$B
W$B
W$B
V9B
V9B
VB
VSB
W?B
V9B
XEB
XEB
XEB
XEB
XEB
X_B
Y1B
Y1B
Y1B
XyB
YeB
YB
ZkB
\]B
\]B
\]B
\]B
]dB
]dB
]IB
]IB
^5B
]/B
]dB
]dB
]dB
]dB
]dB
^jB
^�B
^jB
^jB
_VB
_pB
`\B
`vB
`vB
`vB
a|B
`�B
a|B
bhB
bhB
bhB
bhB
bNB
bhB
b�B
bhB
bNB
bhB
bNB
b�B
b�B
a|B
b�B
c�B
dtB
dtB
dtB
d�B
dtB
dtB
d�B
d�B
d�B
dtB
ezB
ezB
ezB
ezB
ezB
ezB
ezB
e`B
ezB
ezB
e�B
e�B
e�B
ezB
f�B
f�B
e�B
e�B
e�B
f�B
f�B
f�B
g�B
g�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
hsB
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
jB
i�B
i�B
i�B
j�B
j�B
j�B
k�B
j�B
k�B
k�B
j�B
k�B
k�B
j�B
k�B
k�B
k�B
k�B
k�B
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
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
q�B
r�B
s�B
r�B
s�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
u�B
t�B
t�B
t�B
uB
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
v�B
v�B
w�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.18(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201708120039212017081200392120170812003921201806221317232018062213172320180622131723201804050719352018040507193520180405071935  JA  ARFMdecpA19c                                                                20170808093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170808003514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170808003515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170808003516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170808003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170808003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170808003516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170808003516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170808003517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170808003517                      G�O�G�O�G�O�                JA  ARUP                                                                        20170808005712                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170808153308  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20170809000000  CF  PSAL_ADJUSTED_QCD�� D�  G�O�                JM  ARCAJMQC2.0                                                                 20170811153921  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170811153921  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221935  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041723  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                