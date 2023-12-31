CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-03T20:16:08Z AOML 3.0 creation; 2016-05-31T19:14:46Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20151103201608  20160531121446  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_129                   2C  D   APEX                            5368                            041511                          846 @�{��ݩ1   @�{ޘ���@3̋C���dm�hr�!1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�fDy� D� D�9�D��3D��3D�fD�FfD��fD�� D� D�6fD�� D��3D���D�<�Dڐ D���D�� D�9�D�fD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@x��@�G�@�G�A��A<��A\��A|��A�Q�A��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B�\B'�\B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw�\B(�B��{B��{B��{B�aHB��{B��{B��{B��{B��{B��{B�ǮB��{B�aHB�aHB��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce��Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�)Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds��Dy��D�	HD�2�D�|{D��{D��D�?�D��D��HD�	HD�/�D�yHD��{D��D�6DډHD��D��HD�2�D��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�
=A�oA�oA�JA�VA�VA�VA�VA�%A��mA�AڬA�p�A�Aٺ^AًDA�jA�`BA�S�A�33AجA�?}A׬A�K�A�5?A��A���A�JAП�A�S�A�ZA�AˬA��A�$�A�?}A�1'A���A��`AŬA���A�A��;A�C�A��A��-A�^5A���A��FA��A�5?A��A���A��+A��yA���A�hsA�JA��\A���A��mA��^A��A�bA��+A�bNA��yA�/A�r�A��A�K�A���A�|�A���A��;A�S�A�A��-A�-A��FA���A�ƨA�^5A�
=A���A���A�"�A�-A�=qA�=qA�/A�ĜA�ZA���A���A�t�A��A��-A��uA� �A��A���A�ZA��A���A�K�A;dA{��Ay�wAw�TAu+At-As?}Aqt�AoVAl��Aj��Ai�Ag��AfbAe�^AeS�AaoA^A\{AZA�AX^5AW��AW"�AVVAU&�AR��AQ%AP  AMoAKp�AG��AF~�AFJAD�AC�AA�A?x�A>�A>�+A=��A;��A:ĜA8=qA7/A6~�A6JA4�9A2��A1"�A.�/A.ZA-�7A,��A+��A*$�A(ȴA&��A%t�A#/A"1A!oA I�A|�A7LAȴA+A{A/A�A�A��A7LA��A��A��A��Az�A?}Ar�A�\AhsAbNA1A��A��A��A
��A	
=A�uA�#A^5AffAVA5?A ��@���@���@��@���@�7L@� �@�+@�O�@�|�@�M�@� �@���@�@��@�r�@�dZ@@�V@���@�t�@��@��@�l�@�O�@��;@߶F@�S�@��@އ+@�O�@ܓu@�b@���@۾w@�l�@�"�@��H@���@���@ؼj@ם�@֗�@թ�@Ԭ@ӕ�@��H@Ѳ-@���@ύP@�n�@̬@�dZ@�=q@��`@��
@�K�@��@�-@�x�@���@�A�@���@ÍP@�"�@�o@��y@\@�^5@�x�@�Z@�"�@���@��\@�$�@��@�@��-@�O�@�&�@��@�b@�|�@�o@���@�\)@���@���@��@���@�p�@���@��@�M�@�/@�Ĝ@���@�J@��@��@��@�A�@��m@���@�t�@��\@�5?@�{@�@��@��7@�X@�/@��`@�A�@��@�ȴ@��R@��+@�ff@�=q@�$�@��@���@�`B@��9@�r�@�z�@�Q�@��P@�33@�+@�+@���@��!@�=q@��@��@���@��h@�Ĝ@�Z@���@��P@��P@�t�@�33@�ȴ@�^5@�5?@�@���@�7L@���@��`@���@�Ĝ@��@�bN@�(�@��;@�|�@��y@��R@���@���@�v�@�=q@��@���@��h@�/@���@���@��u@�r�@�b@��@��;@��
@��@�l�@�"�@�"�@�
=@��@��!@���@�^5@�@���@�x�@��@���@��`@���@���@�r�@�Z@���@��@��u@�z�@�S�@���@���@��@��@���@���@��D@�j@�(�@�ƨ@��@�C�@�o@�@��@��y@��H@���@���@�^5@�^5@�V@�E�@��-@��@�O�@��@��`@��j@��u@�Z@�bN@��u@�`B@�7L@�&�@���@��9@��@��@�bN@�(�@�b@�  @���@�S�@�+@��y@�^5@��@�hs@�?}@���@��@���@��@�j@�9X@�  @�33@��@���@��R@��\@�E�@��#@��^@��7@�x�@�p�@�G�@��@�z�@�(�@�ƨ@���@�K�@��@�@��@��!@�ff@��@��m@��@v�R@m�@d�@W��@N��@G�@@�9@:J@2��@,�@(b@"�@@J@��@��@Ĝ@�@
�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111A�
=A�oA�oA�JA�VA�VA�VA�VA�%A��mA�AڬA�p�A�Aٺ^AًDA�jA�`BA�S�A�33AجA�?}A׬A�K�A�5?A��A���A�JAП�A�S�A�ZA�AˬA��A�$�A�?}A�1'A���A��`AŬA���A�A��;A�C�A��A��-A�^5A���A��FA��A�5?A��A���A��+A��yA���A�hsA�JA��\A���A��mA��^A��A�bA��+A�bNA��yA�/A�r�A��A�K�A���A�|�A���A��;A�S�A�A��-A�-A��FA���A�ƨA�^5A�
=A���A���A�"�A�-A�=qA�=qA�/A�ĜA�ZA���A���A�t�A��A��-A��uA� �A��A���A�ZA��A���A�K�A;dA{��Ay�wAw�TAu+At-As?}Aqt�AoVAl��Aj��Ai�Ag��AfbAe�^AeS�AaoA^A\{AZA�AX^5AW��AW"�AVVAU&�AR��AQ%AP  AMoAKp�AG��AF~�AFJAD�AC�AA�A?x�A>�A>�+A=��A;��A:ĜA8=qA7/A6~�A6JA4�9A2��A1"�A.�/A.ZA-�7A,��A+��A*$�A(ȴA&��A%t�A#/A"1A!oA I�A|�A7LAȴA+A{A/A�A�A��A7LA��A��A��A��Az�A?}Ar�A�\AhsAbNA1A��A��A��A
��A	
=A�uA�#A^5AffAVA5?A ��@���@���@��@���@�7L@� �@�+@�O�@�|�@�M�@� �@���@�@��@�r�@�dZ@@�V@���@�t�@��@��@�l�@�O�@��;@߶F@�S�@��@އ+@�O�@ܓu@�b@���@۾w@�l�@�"�@��H@���@���@ؼj@ם�@֗�@թ�@Ԭ@ӕ�@��H@Ѳ-@���@ύP@�n�@̬@�dZ@�=q@��`@��
@�K�@��@�-@�x�@���@�A�@���@ÍP@�"�@�o@��y@\@�^5@�x�@�Z@�"�@���@��\@�$�@��@�@��-@�O�@�&�@��@�b@�|�@�o@���@�\)@���@���@��@���@�p�@���@��@�M�@�/@�Ĝ@���@�J@��@��@��@�A�@��m@���@�t�@��\@�5?@�{@�@��@��7@�X@�/@��`@�A�@��@�ȴ@��R@��+@�ff@�=q@�$�@��@���@�`B@��9@�r�@�z�@�Q�@��P@�33@�+@�+@���@��!@�=q@��@��@���@��h@�Ĝ@�Z@���@��P@��P@�t�@�33@�ȴ@�^5@�5?@�@���@�7L@���@��`@���@�Ĝ@��@�bN@�(�@��;@�|�@��y@��R@���@���@�v�@�=q@��@���@��h@�/@���@���@��u@�r�@�b@��@��;@��
@��@�l�@�"�@�"�@�
=@��@��!@���@�^5@�@���@�x�@��@���@��`@���@���@�r�@�Z@���@��@��u@�z�@�S�@���@���@��@��@���@���@��D@�j@�(�@�ƨ@��@�C�@�o@�@��@��y@��H@���@���@�^5@�^5@�V@�E�@��-@��@�O�@��@��`@��j@��u@�Z@�bN@��u@�`B@�7L@�&�@���@��9@��@��@�bN@�(�@�b@�  @���@�S�@�+@��y@�^5@��@�hs@�?}@���@��@���@��@�j@�9X@�  @�33@��@���@��R@��\@�E�@��#@��^@��7@�x�@�p�@�G�@��@�z�@�(�@�ƨ@���@�K�@��@�@��@��!@�ffG�O�@��m@��@v�R@m�@d�@W��@N��@G�@@�9@:J@2��@,�@(b@"�@@J@��@��@Ĝ@�@
�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�JB�DB�DB�=B�=B�=B�{B��B��B��B�B�ZB��B$�B:^BA�BD�BD�BD�BJ�Bv�B�9B��BBBbBuB �B2-B8RB=qB=qBA�BF�BG�BQ�B]/BcTBffBl�Bu�Bv�Bs�Bm�Bk�BcTBZBP�BJ�BF�BD�BB�B?}B9XB/B-B"�B�BPBB�B�wB��B�BdZBq�B}�BL�B'�B49B=qB9XB/B$�B�BuB��B�B�fB�#B�'B�Be`BdZBe`BdZBgmBp�Bt�Bt�Br�Bm�B\)BT�BQ�BL�BE�B,B�BhB  B
�B
�#B
��B
�LB
�B
��B
q�B
`BB
VB
:^B
9XB
8RB
,B
�B
DB	��B	�B	�B	��B	��B	ȴB	�-B	��B	�{B	�7B	}�B	w�B	s�B	k�B	^5B	N�B	C�B	;dB	.B	"�B	uB	VB	
=B	B��B��B�B�mB�ZB�BB�B��B��B��BɺBƨBÖB�}B�^B�?B�-B�B�B��B��B��B��B��B�hB�\B�VB�JB�7B�1B�%B�%B�B�B~�B|�B{�B{�Bz�Bx�Bw�Bu�Bs�Br�Bw�B�B�+B�=B�DB�DB�7B�B� B� B}�Bz�Bz�B{�Bz�Bz�Bz�Bz�Bz�Bz�Bz�By�By�By�By�Bz�Bz�B{�B|�B|�B|�B|�B|�Bz�Bv�Bs�Br�Bp�Bm�Bk�Bn�Bp�Bp�Bp�Bo�Bo�Bp�Br�Bt�Bu�Bu�Bv�Bw�Bw�Bw�Bz�B|�B�B�B�+B�1B�=B�DB�VB�hB�bB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�?B�LB�^B�}BÖBɺB�#B�;B�;B�B��B��B	B	1B	VB	\B	uB	�B	�B	�B	!�B	$�B	%�B	'�B	'�B	-B	33B	7LB	9XB	=qB	C�B	F�B	H�B	I�B	K�B	L�B	Q�B	S�B	VB	XB	ZB	[#B	[#B	]/B	_;B	cTB	cTB	dZB	ffB	hsB	k�B	p�B	s�B	w�B	y�B	z�B	}�B	�B	�B	�B	�B	�B	�B	�+B	�1B	�1B	�1B	�DB	�VB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�?B	�FB	�FB	�FB	�LB	�XB	�dB	�qB	�wB	�}B	��B	��B	��B	��B	��B	B	ÖB	ŢB	ǮB	ȴB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�HB	�NB	�ZB	�fB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
	7B

=B

=B
DB
�B
\B
�B
�B
#�B
,B
33B
9XB
?}B
E�B
K�B
Q�B
XB
aHB
gmB
k�B
o�B
q�B
t�B
w�B
z�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111B�]B�WB�UB�OB�KB�MB��B��B�B��B� B�kB��B$�B:pBA�BD�BD�BD�BJ�Bv�B�LB��B2B,BvB�B �B2AB8gB=�B=�BA�BF�BG�BRB]BBcmBf{Bl�Bu�Bv�Bs�Bm�Bk�BcnBZ3BP�BJ�BF�BD�BB�B?�B9pB/1B-$B"�B�BfB#B��B��B��B�5BdjBq�B~
BL�B(B4PB=�B9lB//B$�B�B�B��B�B�{B�;B�:B�.BevBdoBeuBdnBg�Bp�Bt�Bt�Br�Bm�B\?BUBRBL�BE�B, B�B�B B
�B
�7B
��B
�fB
�B
��B
q�B
`]B
VB
:}B
9sB
8mB
,&B
�B
cB	��B	�B	�6B	��B	��B	��B	�PB	��B	��B	�ZB	~B	w�B	s�B	k�B	^ZB	N�B	C�B	;�B	.;B	"�B	�B	}B	
eB	DB�B��B�B�B�B�kB�IB�"B�	B��B��B��B��B��B��B�kB�ZB�HB�3B�B��B��B��B��B��B��B��B�xB�gB�aB�TB�TB�BB�3B(B}B|B|B{ByBw�Bu�Bs�Br�Bw�B�5B�ZB�kB�tB�rB�hB�LB�/B�.B~"B{B{B|B{B{B{B{B{B{B{BzBz
BzBzB{B{B|B}B}B}B}B}B{Bv�Bs�Br�Bp�Bm�Bk�Bn�Bp�Bp�Bp�Bo�Bo�Bp�Br�Bt�Bu�Bu�Bv�Bw�Bw�Bx B{B}B�6B�IB�[B�aB�lB�tB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�)B�(B�+B�6B�TB�kB�yB��B��BÿB��B�NB�hB�eB��B��B�B	0B	\B	�B	�B	�B	�B	�B	�B	!�B	%B	&B	(B	(B	-6B	3[B	7tB	9~B	=�B	C�B	F�B	H�B	I�B	K�B	L�B	RB	T"B	V+B	X8B	ZEB	[GB	[JB	]VB	_dB	c}B	czB	d�B	f�B	h�B	k�B	p�B	s�B	w�B	zB	{B	~B	�*B	�*B	�2B	�1B	�0B	�GB	�PB	�TB	�XB	�XB	�jB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�"B	�&B	�+B	�,B	�7B	�FB	�GB	�LB	�RB	�bB	�hB	�kB	�kB	�qB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	²B	úB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	� B	� B	�3B	�AB	�@B	�HB	�LB	�QB	�ZB	�^B	�jB	�rB	�|B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B
  B
 %B
 $B
 "B
'B
'B
0B
-B
4B
=B
AB
GB
HB
HB
IB
JB
DB
MB
NB
WB
UB
SB
RB
	XB

aB

_B
dG�O�B
~B
�B
�B
#�B
,'B
3UB
9wB
?�B
E�B
K�B
RB
X1B
agB
g�B
k�B
o�B
q�B
t�B
w�B
{B
~1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.21 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214462016053112144620160531121446  AO  ARCAADJP                                                                    20151103201608    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151103201608  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151103201608  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121446  IP                  G�O�G�O�G�O�                