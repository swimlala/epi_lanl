CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-04-24T03:48:28Z creation;2023-04-24T03:48:29Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230424034828  20230424040720  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�%����u1   @�%��o�@/4�j~���c�$�/1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  BxffB�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�33B�ffB���B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C33C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CG�fCI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fD  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ Dü�D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۃ3D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@(�@r�\@�G�@�z�A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_�\Bg�\Bo(�Bw�\B(�B�aHB�aHB��{B��{B��{B��{B��{B��{B��{B��{B��{B�ǮB�aHB��{B�ǮB���B�aHBǔ{B˔{Bϔ{BӔ{B�ǮB�ǮB�aHB�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�pC�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE��CG��CI��CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�Dur�Du�Dvr�Dv�Dwr�Dw�Dxr�Dx�Dyr�Dy�Dzr�Dz�D{r�D{�D|r�D|�D}r�D}�D~x�D~�Dr�D�D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��D��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD¹HD��HD�9HD�yHDöD��HD�9HD�yHDĹHD��HD�9HD�yHDŹHD��HD�9HD�yHDƹHD��HD�9HD�yHDǹHD��HD�9HD�yHDȹHD��HD�9HD�yHDɹHD��HD�9HD�yHDʹHD��HD�9HD�yHD˹HD��HD�9HD�yHD̹HD��HD�9HD�yHD͹HD��HD�9HD�yHDιHD��HD�9HD�yHDϹHD��HD�9HD�yHDйHD��HD�9HD�yHDѹHD��HD�9HD�yHDҹHD��HD�9HD�yHDӹHD��HD�9HD�yHDԹHD��HD�9HD�yHDչHD��HD�9HD�yHDֹHD��HD�9HD�yHD׹HD��HD�9HD�yHDعHD��HD�9HD�yHDٹHD��HD�9HD�yHDڹHD��HD�9HD�|{D۹HD��HD�9HD�yHDܹHD��HD�9HD�yHDݹHD��HD�9HD�yHD޹HD��HD�9HD�yHD߹HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��aA��-AϾwAϹ�AϵAϚ7Aω�A�}"A�}VA�|PA�x�A�v�A�sA�rGA�sA�sMA�r|A�l�A�`vA�,qA��}A�l"A�T,A�;dA�&A�A��A�� A��[A͵�AͲ�Aͬ=A�C�A��A�aA˫6A�S&Aǖ�A�u�AǑ4A�dZA�(�A��[A��Aŋ�A�4nA���A�	A�A�u�A��jA��	A��mA���A�ݘA�� A���A�($A�уA�pA���A��gA���A��eA��ZA�� A�K�A���A�?A��A���A�x�A�&�A�k�A��A��~A���A�C�A�f�A�}�A��A��4A� �A��A�"A���A���A���A�uA���A�"hA���A�<A�$A���A�@OA�A A���A�N�A�B�A}K^Ay��Aw�Av��Au�6At��As��Am?}Ah��Ag�hA_��AYATffAQ��AP��AM��AG0�AEL0ADi�AC�qAA�*AAI�A@��A?:�A>�3A=�A<4A:�A9�+A7�A2�A/4A+�A*��A(�A&��A$GA"ߤA!�/A <�A��A@OA^�A�A-�A=qA�?AߤA�A��A�hA��A�'A1�A��A�A�dACA	��AkQAdZA�0A�zA��A~�A�=A�HA&�AHA]dA��AbA�.AںA�A]dA(�A l�@���@���@�O�@��@��}@���@�F�@���@�`�@�Q�@�;�@�K^@�v�@�@�t@�f�@�Vm@�J@���@��K@���@�M@��@@�� @�9�@�!@��@��f@�]d@��@�F�@�>B@��@��)@莊@�R�@�|�@��@慈@�ݘ@�p�@�o�@�<6@��@�{@���@��'@��@��@ऩ@��@߉7@޹$@�1�@�+k@�&�@�O@ݠ�@��}@�˒@ܡb@��	@�'R@ٝ�@��@�-�@�	�@�A @�ߤ@֭�@ֻ�@ֵ@�|�@գn@�h
@�8�@�!�@�?}@��@��@ї�@�Y�@Ѹ�@�\)@ѧ�@�҉@�9�@���@҉�@�l�@���@���@���@���@�O@�B�@��}@̣�@�e�@�_@�ff@ˍP@��p@�q@ɬq@�`B@�4@�@�G�@�5�@Ț@�H@��@Ʊ�@�J�@���@�7L@�/�@�X�@�0�@�s�@�H@��9@��^@�?}@�Y�@�RT@�f�@���@� i@��j@��@��@�l"@��>@���@��@��[@�`�@��@���@���@���@�c�@� �@�|@��@��@�ff@���@��{@�8�@��@�u�@�!@���@���@�T�@���@���@�tT@���@�8�@���@���@�>B@�4�@���@�!@���@�%@���@���@��)@�W?@��@���@�>B@���@�B�@�ѷ@�8�@��t@�%@���@�L0@���@�\�@��@��e@�bN@�e@�G@��9@��{@�f�@�F�@��@�w�@��@���@��}@���@�Vm@�&@��@�ی@��r@�
�@���@�c@�hs@�c�@�e�@�hs@�J#@��@���@�g8@� �@���@���@��~@�(�@��@�V@�&�@��o@���@���@�zx@�A @���@�L0@��@��h@�m]@�!�@���@���@�g8@�:*@��
@���@�|�@��`@���@�%�@��6@��M@�RT@��@��M@���@�S�@�C�@�x@��@��6@���@�hs@�IR@�C@��c@��c@��u@��@��6@�x�@�5�@��@�ȴ@�Ov@���@��z@��H@�]�@�Y@��@��P@�PH@��@��P@�e�@�_@��7@�~�@�|�@�o�@�ѷ@��1@�h
@��@��a@��@��$@��@�!�@��@��@��.@�M�@� �@���@���@��=@���@��Y@��\@�K^@���@��@���@�/�@���@�!�@�j@�|�@���@��-@�O�@�\)@�33@�@�ی@���@��!@�� @�Z�@��@���@�2a@��_@�?@��'@��@���@�� @�j@�e�@�3�@���@��Z@��[@��@���@���@��L@���@�c�@�C-@�D�@�_@�hs@��@��s@���@�V�@�@�@~c @}��@}�@}�h@}B�@|I�@{�k@{��@z�"@z� @z��@y�)@y�@xtT@w�]@w�0@w�4@wP�@w8@w@vff@u��@t�z@tz�@t�$@t��@s�]@s$t@r͟@r	@qw2@q+�@p��@p��@p�5@p�O@pFt@o�@o�W@o��@o
=@n��@n=q@n�<@nB[@mX@l�[@l�e@l�@lm�@l �@k]�@j\�@j8�@i��@h�e@h|�@h_@hV�@hFt@g�A@g�k@gS@f��@f�"@f��@fTa@f$�@e��@e��@ec@e�@d��@c�@cK�@c(@b��@bd�@a�@a@`�4@`N�@_˒@`%�@`1@_�@_(@_g�@_�@^��@]��@]V@\�I@[��@["�@Z҉@Z�@Z�<@Z�@Y�@YIR@X��@X>B@W��@W��@Vں@VZ�@VW�@VJ�@V �@U@@Tu�@T,=@Se�@R��@RJ�@Ru@Q��@Q��@Q[W@Q�@P�`@Pu�@P,=@P/�@Oخ@O��@O�@N�,@N��@N�h@N:*@N_@M��@M@M�S@M7L@L�@L�@L��@Lr�@LI�@Kخ@K�@K�@KS�@J�s@J�L@J	@I��@IV@H��@HK^@H�@G��@Ge�@G9�@F�@F�\@FZ�@Fe@E@E�C@Es�@E0�@D�?@DN�@Ca@B�@B�!@B�L@BYK@A��@A�n@Azx@Ao @Ahs@A7L@@��@@<�@@7@?�f@>�@>�r@>YK@>�@=�@=��@=hs@=?}@=+@<Ĝ@<�Y@<�@;X�@:��@:��@:l�@9�@9<6@8�5@8l"@89X@8~@7�Q@7��@7K�@7)_@6��@6}V@6:*@6�@6J@6@5�@5�^@5��@50�@4��@4I�@4	�@3�@3��@3�[@3�k@3��@3A�@2�'@2p;@2J�@2-@1�.@1��@1`B@1��@1��@1-w@1�@0�f@0Ɇ@0D�@/�Q@/��@/��@/��@/n/@/�@.�]@.ȴ@.�1@.J�@-��@-�3@-�M@-8�@-@,��@,�@+��@+��@+S�@*��@*��@*n�@*ff@*Ov@*($@)�.@)c@)(�@(�@(�u@(M@'�r@'�@'��@'qv@'$t@&�@&v�@&3�@%��@%��@%�=@%m]@%`B@%:�@%�@%�@$�@$Ɇ@$�e@$�u@$j@$-�@#�]@#�4@#@"��@"��@!��@!k�@ �@ c�@خ@{J@qv@s@U�@$t@@S@�@p;@$�@��@�@Dg@bN@<�@%�@�@x@�]@�@�@�6@X�@�@��@~�@YK@M�@:*@ �@��@��@��@u�@S&@�P@��@g8@b@�[@W?@��@i�@Z�@1�@�@�@^�@:�@;@��@�p@�@�[@�$@%�@��@��@�:@dZ@=@9�@/�@�@�h@GE@6�@+k@
�@��@��@�j@�@�~@IR@+�@	l@�E@�@!@G@��@�}@�a@��@�@�V@��@]�@P�@9�@��@�X@ȴ@�'@��@u%@ff@M�@0U@O@��@�@��@k�@J�@q@��@�D@Xy@-�@�@�&@�0@|�@s@H�@�@�@
��@
�h@
z@
^5@
GE@
6�@
-@
J@	�@	��@	ϫ@	a�@	Dg@	+�@��@~(@'R@�@�6@��@o�@U�@9�@�@�@�@�@-@	@	@J@��@ϫ@��@f�@=�@2a@*0@�@��@��@�u@w�@[�@1'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��aA��-AϾwAϹ�AϵAϚ7Aω�A�}"A�}VA�|PA�x�A�v�A�sA�rGA�sA�sMA�r|A�l�A�`vA�,qA��}A�l"A�T,A�;dA�&A�A��A�� A��[A͵�AͲ�Aͬ=A�C�A��A�aA˫6A�S&Aǖ�A�u�AǑ4A�dZA�(�A��[A��Aŋ�A�4nA���A�	A�A�u�A��jA��	A��mA���A�ݘA�� A���A�($A�уA�pA���A��gA���A��eA��ZA�� A�K�A���A�?A��A���A�x�A�&�A�k�A��A��~A���A�C�A�f�A�}�A��A��4A� �A��A�"A���A���A���A�uA���A�"hA���A�<A�$A���A�@OA�A A���A�N�A�B�A}K^Ay��Aw�Av��Au�6At��As��Am?}Ah��Ag�hA_��AYATffAQ��AP��AM��AG0�AEL0ADi�AC�qAA�*AAI�A@��A?:�A>�3A=�A<4A:�A9�+A7�A2�A/4A+�A*��A(�A&��A$GA"ߤA!�/A <�A��A@OA^�A�A-�A=qA�?AߤA�A��A�hA��A�'A1�A��A�A�dACA	��AkQAdZA�0A�zA��A~�A�=A�HA&�AHA]dA��AbA�.AںA�A]dA(�A l�@���@���@�O�@��@��}@���@�F�@���@�`�@�Q�@�;�@�K^@�v�@�@�t@�f�@�Vm@�J@���@��K@���@�M@��@@�� @�9�@�!@��@��f@�]d@��@�F�@�>B@��@��)@莊@�R�@�|�@��@慈@�ݘ@�p�@�o�@�<6@��@�{@���@��'@��@��@ऩ@��@߉7@޹$@�1�@�+k@�&�@�O@ݠ�@��}@�˒@ܡb@��	@�'R@ٝ�@��@�-�@�	�@�A @�ߤ@֭�@ֻ�@ֵ@�|�@գn@�h
@�8�@�!�@�?}@��@��@ї�@�Y�@Ѹ�@�\)@ѧ�@�҉@�9�@���@҉�@�l�@���@���@���@���@�O@�B�@��}@̣�@�e�@�_@�ff@ˍP@��p@�q@ɬq@�`B@�4@�@�G�@�5�@Ț@�H@��@Ʊ�@�J�@���@�7L@�/�@�X�@�0�@�s�@�H@��9@��^@�?}@�Y�@�RT@�f�@���@� i@��j@��@��@�l"@��>@���@��@��[@�`�@��@���@���@���@�c�@� �@�|@��@��@�ff@���@��{@�8�@��@�u�@�!@���@���@�T�@���@���@�tT@���@�8�@���@���@�>B@�4�@���@�!@���@�%@���@���@��)@�W?@��@���@�>B@���@�B�@�ѷ@�8�@��t@�%@���@�L0@���@�\�@��@��e@�bN@�e@�G@��9@��{@�f�@�F�@��@�w�@��@���@��}@���@�Vm@�&@��@�ی@��r@�
�@���@�c@�hs@�c�@�e�@�hs@�J#@��@���@�g8@� �@���@���@��~@�(�@��@�V@�&�@��o@���@���@�zx@�A @���@�L0@��@��h@�m]@�!�@���@���@�g8@�:*@��
@���@�|�@��`@���@�%�@��6@��M@�RT@��@��M@���@�S�@�C�@�x@��@��6@���@�hs@�IR@�C@��c@��c@��u@��@��6@�x�@�5�@��@�ȴ@�Ov@���@��z@��H@�]�@�Y@��@��P@�PH@��@��P@�e�@�_@��7@�~�@�|�@�o�@�ѷ@��1@�h
@��@��a@��@��$@��@�!�@��@��@��.@�M�@� �@���@���@��=@���@��Y@��\@�K^@���@��@���@�/�@���@�!�@�j@�|�@���@��-@�O�@�\)@�33@�@�ی@���@��!@�� @�Z�@��@���@�2a@��_@�?@��'@��@���@�� @�j@�e�@�3�@���@��Z@��[@��@���@���@��L@���@�c�@�C-@�D�@�_@�hs@��@��s@���@�V�@�@�@~c @}��@}�@}�h@}B�@|I�@{�k@{��@z�"@z� @z��@y�)@y�@xtT@w�]@w�0@w�4@wP�@w8@w@vff@u��@t�z@tz�@t�$@t��@s�]@s$t@r͟@r	@qw2@q+�@p��@p��@p�5@p�O@pFt@o�@o�W@o��@o
=@n��@n=q@n�<@nB[@mX@l�[@l�e@l�@lm�@l �@k]�@j\�@j8�@i��@h�e@h|�@h_@hV�@hFt@g�A@g�k@gS@f��@f�"@f��@fTa@f$�@e��@e��@ec@e�@d��@c�@cK�@c(@b��@bd�@a�@a@`�4@`N�@_˒@`%�@`1@_�@_(@_g�@_�@^��@]��@]V@\�I@[��@["�@Z҉@Z�@Z�<@Z�@Y�@YIR@X��@X>B@W��@W��@Vں@VZ�@VW�@VJ�@V �@U@@Tu�@T,=@Se�@R��@RJ�@Ru@Q��@Q��@Q[W@Q�@P�`@Pu�@P,=@P/�@Oخ@O��@O�@N�,@N��@N�h@N:*@N_@M��@M@M�S@M7L@L�@L�@L��@Lr�@LI�@Kخ@K�@K�@KS�@J�s@J�L@J	@I��@IV@H��@HK^@H�@G��@Ge�@G9�@F�@F�\@FZ�@Fe@E@E�C@Es�@E0�@D�?@DN�@Ca@B�@B�!@B�L@BYK@A��@A�n@Azx@Ao @Ahs@A7L@@��@@<�@@7@?�f@>�@>�r@>YK@>�@=�@=��@=hs@=?}@=+@<Ĝ@<�Y@<�@;X�@:��@:��@:l�@9�@9<6@8�5@8l"@89X@8~@7�Q@7��@7K�@7)_@6��@6}V@6:*@6�@6J@6@5�@5�^@5��@50�@4��@4I�@4	�@3�@3��@3�[@3�k@3��@3A�@2�'@2p;@2J�@2-@1�.@1��@1`B@1��@1��@1-w@1�@0�f@0Ɇ@0D�@/�Q@/��@/��@/��@/n/@/�@.�]@.ȴ@.�1@.J�@-��@-�3@-�M@-8�@-@,��@,�@+��@+��@+S�@*��@*��@*n�@*ff@*Ov@*($@)�.@)c@)(�@(�@(�u@(M@'�r@'�@'��@'qv@'$t@&�@&v�@&3�@%��@%��@%�=@%m]@%`B@%:�@%�@%�@$�@$Ɇ@$�e@$�u@$j@$-�@#�]@#�4@#@"��@"��@!��@!k�@ �@ c�@خ@{J@qv@s@U�@$t@@S@�@p;@$�@��@�@Dg@bN@<�@%�@�@x@�]@�@�@�6@X�@�@��@~�@YK@M�@:*@ �@��@��@��@u�@S&@�P@��@g8@b@�[@W?@��@i�@Z�@1�@�@�@^�@:�@;@��@�p@�@�[@�$@%�@��@��@�:@dZ@=@9�@/�@�@�h@GE@6�@+k@
�@��@��@�j@�@�~@IR@+�@	l@�E@�@!@G@��@�}@�a@��@�@�V@��@]�@P�@9�@��@�X@ȴ@�'@��@u%@ff@M�@0U@O@��@�@��@k�@J�@q@��@�D@Xy@-�@�@�&@�0@|�@s@H�@�@�@
��@
�h@
z@
^5@
GE@
6�@
-@
J@	�@	��@	ϫ@	a�@	Dg@	+�@��@~(@'R@�@�6@��@o�@U�@9�@�@�@�@�@-@	@	@J@��@ϫ@��@f�@=�@2a@*0@�@��@��@�u@w�@[�@1'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	��B	��B	�MB	��B	�?B	�FB	��B	�fB	��B	�*B	��B
oB
gB
	�B
�B
�B
�B
7B
~B
#:B
#�B
 �B
�B
B
IB
�B
# B
%�B
*eB
/B
*B
-�B
;�B
H�B
|�B
��B
�B
��B
�4B
�B�B�B.B�B)*B1Bn}B�DB�tB�^B��BjBCB�BLJBESBI7BM6BZBN<BB[B4TB%�B(
B#nB�B�BB"�B&�B��B��B��B�/B��B��B��B�B�;B��B�DB��By	Bf�BX�BIRB>B./B
B
�HB
�mB
ÖB
��B
�BB
{�B
O\B
+6B
qB
�B	�FB	�B	��B	ݘB	��B	ΥB	��B	�_B	��B	m)B	OvB	?B	5tB	.�B	#nB	�B	aB��B�B��B�vB�oB�B�B�B�mB�B�B޸B�_B�YB�gB�aB��B��B��B��B�B�B�/B��B�wB�B�B�
B�*B�CB��B�UB�?B	�B	EB	~B	oB	�B	�B�lB�xB�XB�PB�B�}B��B�%B��BרB��B�*B��B�*B	
#B	uB	�B	 �B	'8B	�B	�B	�B	IB	!�B	 �B	�B	�B	OB	B	
B	�B	�B	(�B	*KB	#�B	,WB	0�B	%FB	/�B	3B	8B	9�B	;B	B[B	PHB	WsB	Y�B	X�B	Y1B	YB	c:B	g�B	f2B	e�B	iB	j�B	i�B	k�B	hXB	iyB	l�B	m�B	m)B	pB	qB	rB	s3B	u?B	u%B	w�B	w�B	y�B	{0B	}�B	~�B	~�B	�OB	��B	��B	��B	��B	�aB	��B	�(B	��B	�tB	�EB	�mB	�%B	�%B	�KB	�=B	�dB	�~B	�BB	��B	��B	��B	�YB	��B	��B	�pB	��B	��B	��B	��B	�zB	�;B	�1B	�RB	ȴB	�KB	ȀB	ȴB	�B	ΥB	͟B	ϫB	�B	��B	ҽB	�2B	յB	��B	�{B	ּB	��B	�
B	ּB	��B	�=B	��B	�xB	�#B	ؓB	��B	خB	רB	��B	ٴB	�xB	�;B	޸B	�B	��B	�B	՛B	��B	�7B	��B	��B	��B	�B	��B	�LB	�B	�B	�B	�yB	�B	��B	�B	�B	�eB	�B	��B	�B	��B	�wB	��B	�cB	�B	�B	�UB	�'B	��B	�B	��B	��B	�ZB	��B	��B	�`B	�fB	��B	��B	��B	�8B	��B	�0B	��B	�B	��B	��B	��B	��B	��B	��B
 �B
UB
[B
-B
B
�B
gB
�B
�B
�B
�B
B
1B
KB
�B
�B
	B
	�B
	�B

	B

	B

XB
�B
~B
PB
<B
�B
�B
�B
�B
�B
NB
oB
TB
oB
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
 B
B
�B
�B
�B
aB
B
MB
�B
mB
sB
�B
�B
�B

B
�B
EB
�B
�B
�B
B
B
�B
�B
#B
#B
qB
�B
�B
xB
�B
IB
�B
�B
B
jB
�B
�B
�B
�B
�B
!�B
#B
#�B
"�B
$B
$tB
#�B
#nB
"�B
"hB
"4B
$&B
%,B
%�B
%`B
&�B
(�B
)yB
)�B
(�B
&�B
%�B
%zB
(>B
)DB
*KB
)DB
(�B
(�B
'�B
(sB
*B
)_B
)DB
(�B
(�B
*KB
,"B
+6B
+6B
+�B
,qB
-B
/iB
4B
5?B
6`B
5�B
4B
5?B
5ZB
3�B
1�B
1'B
2aB
5B
8B
9	B
:*B
:�B
:�B
<PB
;B
=<B
=�B
>�B
?.B
?.B
=�B
<6B
;�B
;0B
;JB
;dB
<B
<�B
<�B
<�B
=VB
>BB
@OB
?}B
>�B
@�B
CB
B�B
B�B
B'B
BAB
C�B
CaB
C{B
CGB
B�B
B�B
BuB
B'B
B�B
DMB
E�B
E�B
E9B
D�B
D3B
C�B
D3B
D�B
E�B
F?B
E�B
E�B
FB
FYB
GB
GzB
HB
H1B
I�B
J�B
JrB
J�B
LB
MB
M�B
M�B
NB
NVB
N"B
NB
NB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
NVB
P.B
RoB
RTB
R�B
R�B
R�B
RoB
RoB
R�B
RoB
R�B
S�B
S�B
S�B
S�B
S�B
TB
TaB
TB
TaB
T�B
U�B
U�B
U�B
U�B
U�B
VB
VSB
V�B
V�B
V�B
V�B
V9B
U�B
UgB
T�B
TB
S�B
SB
R�B
UB
VB
UgB
U�B
X_B
X�B
X_B
YB
X�B
X�B
Y�B
Y�B
Y�B
ZQB
[#B
Z�B
Z�B
Z�B
Z�B
[	B
[WB
[WB
[�B
[=B
[�B
[�B
[�B
[�B
\B
\)B
]/B
\�B
\�B
\�B
\�B
]IB
]�B
]dB
]B
\]B
]�B
_;B
_�B
`vB
aHB
aHB
a-B
abB
aHB
a|B
a�B
a-B
a-B
`�B
`\B
`'B
`'B
`�B
abB
bhB
bhB
bhB
b�B
b�B
b�B
c:B
c�B
c�B
c�B
d@B
dtB
d�B
e,B
e�B
e�B
ffB
ffB
f�B
f�B
f�B
gB
gB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h>B
h
B
h
B
h$B
hXB
h�B
i*B
jeB
jB
jeB
jB
jB
j�B
kB
kB
kQB
k�B
kkB
k�B
k�B
lWB
l�B
l�B
l�B
m�B
nB
nIB
ncB
n/B
nB
ncB
ncB
n�B
n}B
n�B
oB
o5B
o5B
oB
oB
o5B
oB
oB
oOB
pUB
p�B
p�B
qB
qAB
q'B
qB
qB
qAB
qvB
qvB
qAB
qAB
qAB
q�B
q�B
q�B
q�B
rGB
r-B
raB
r�B
shB
s�B
s�B
tB
tB
tB
t�B
tTB
tTB
tTB
tTB
tnB
tnB
t�B
uB
u%B
u�B
u�B
v+B
vB
u�B
vFB
u�B
u�B
u�B
u�B
u�B
u�B
v+B
vB
vFB
v�B
v�B
v�B
v�B
v�B
v�B
wB
w�B
w�B
w�B
w�B
x�B
y$B
y�B
y�B
z�B
z�B
{0B
{JB
{�B
{�B
{�B
{�B
|B
|B
|jB
|PB
|PB
|�B
}"B
}VB
}�B
~]B
~�B
~�B
B
.B
�B
�B
�B
�B
�B
� B
�B
�OB
�OB
� B
��B
��B
��B
��B
��B
��B
�oB
�UB
�UB
��B
��B
�AB
�uB
�[B
�uB
�uB
��B
��B
�B
�{B
�{B
�{B
��B
��B
��B
��B
�B
�SB
��B
�B
��B
�%B
�YB
��B
��B
��B
�B
�zB
�_B
�EB
�+B
�EB
��B
�1B
�fB
�fB
��B
��B
��B
��B
��B
�B
�lB
�lB
��B
��B
��B
��B
��B
��B
�#B
�rB
�rB
��B
��B
��B
�xB
�xB
��B
��B
��B
�B
�B
�B
�B
�JB
�dB
�dB
�PB
��B
��B
��B
��B
��B
��B
�<B
�pB
��B
�pB
��B
��B
�(B
�BB
�\B
��B
��B
��B
��B
��B
�HB
�bB
��B
��B
��B
�B
� B
�4B
�hB
��B
��B
��B
��B
��B
�B
� B
�:B
� B
��B
��B
��B
�@B
��B
�B
�FB
�aB
�{B
��B
��B
��B
�B
�B
�MB
��B
�B
�B
�B
�B
�9B
�SB
��B
��B
��B
��B
��B
��B
�?B
�YB
�sB
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	��B	��B	�MB	��B	�?B	�FB	��B	�fB	��B	�*B	��B
oB
gB
	�B
�B
�B
�B
7B
~B
#:B
#�B
 �B
�B
B
IB
�B
# B
%�B
*eB
/B
*B
-�B
;�B
H�B
|�B
��B
�B
��B
�4B
�B�B�B.B�B)*B1Bn}B�DB�tB�^B��BjBCB�BLJBESBI7BM6BZBN<BB[B4TB%�B(
B#nB�B�BB"�B&�B��B��B��B�/B��B��B��B�B�;B��B�DB��By	Bf�BX�BIRB>B./B
B
�HB
�mB
ÖB
��B
�BB
{�B
O\B
+6B
qB
�B	�FB	�B	��B	ݘB	��B	ΥB	��B	�_B	��B	m)B	OvB	?B	5tB	.�B	#nB	�B	aB��B�B��B�vB�oB�B�B�B�mB�B�B޸B�_B�YB�gB�aB��B��B��B��B�B�B�/B��B�wB�B�B�
B�*B�CB��B�UB�?B	�B	EB	~B	oB	�B	�B�lB�xB�XB�PB�B�}B��B�%B��BרB��B�*B��B�*B	
#B	uB	�B	 �B	'8B	�B	�B	�B	IB	!�B	 �B	�B	�B	OB	B	
B	�B	�B	(�B	*KB	#�B	,WB	0�B	%FB	/�B	3B	8B	9�B	;B	B[B	PHB	WsB	Y�B	X�B	Y1B	YB	c:B	g�B	f2B	e�B	iB	j�B	i�B	k�B	hXB	iyB	l�B	m�B	m)B	pB	qB	rB	s3B	u?B	u%B	w�B	w�B	y�B	{0B	}�B	~�B	~�B	�OB	��B	��B	��B	��B	�aB	��B	�(B	��B	�tB	�EB	�mB	�%B	�%B	�KB	�=B	�dB	�~B	�BB	��B	��B	��B	�YB	��B	��B	�pB	��B	��B	��B	��B	�zB	�;B	�1B	�RB	ȴB	�KB	ȀB	ȴB	�B	ΥB	͟B	ϫB	�B	��B	ҽB	�2B	յB	��B	�{B	ּB	��B	�
B	ּB	��B	�=B	��B	�xB	�#B	ؓB	��B	خB	רB	��B	ٴB	�xB	�;B	޸B	�B	��B	�B	՛B	��B	�7B	��B	��B	��B	�B	��B	�LB	�B	�B	�B	�yB	�B	��B	�B	�B	�eB	�B	��B	�B	��B	�wB	��B	�cB	�B	�B	�UB	�'B	��B	�B	��B	��B	�ZB	��B	��B	�`B	�fB	��B	��B	��B	�8B	��B	�0B	��B	�B	��B	��B	��B	��B	��B	��B
 �B
UB
[B
-B
B
�B
gB
�B
�B
�B
�B
B
1B
KB
�B
�B
	B
	�B
	�B

	B

	B

XB
�B
~B
PB
<B
�B
�B
�B
�B
�B
NB
oB
TB
oB
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
 B
B
�B
�B
�B
aB
B
MB
�B
mB
sB
�B
�B
�B

B
�B
EB
�B
�B
�B
B
B
�B
�B
#B
#B
qB
�B
�B
xB
�B
IB
�B
�B
B
jB
�B
�B
�B
�B
�B
!�B
#B
#�B
"�B
$B
$tB
#�B
#nB
"�B
"hB
"4B
$&B
%,B
%�B
%`B
&�B
(�B
)yB
)�B
(�B
&�B
%�B
%zB
(>B
)DB
*KB
)DB
(�B
(�B
'�B
(sB
*B
)_B
)DB
(�B
(�B
*KB
,"B
+6B
+6B
+�B
,qB
-B
/iB
4B
5?B
6`B
5�B
4B
5?B
5ZB
3�B
1�B
1'B
2aB
5B
8B
9	B
:*B
:�B
:�B
<PB
;B
=<B
=�B
>�B
?.B
?.B
=�B
<6B
;�B
;0B
;JB
;dB
<B
<�B
<�B
<�B
=VB
>BB
@OB
?}B
>�B
@�B
CB
B�B
B�B
B'B
BAB
C�B
CaB
C{B
CGB
B�B
B�B
BuB
B'B
B�B
DMB
E�B
E�B
E9B
D�B
D3B
C�B
D3B
D�B
E�B
F?B
E�B
E�B
FB
FYB
GB
GzB
HB
H1B
I�B
J�B
JrB
J�B
LB
MB
M�B
M�B
NB
NVB
N"B
NB
NB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
NVB
P.B
RoB
RTB
R�B
R�B
R�B
RoB
RoB
R�B
RoB
R�B
S�B
S�B
S�B
S�B
S�B
TB
TaB
TB
TaB
T�B
U�B
U�B
U�B
U�B
U�B
VB
VSB
V�B
V�B
V�B
V�B
V9B
U�B
UgB
T�B
TB
S�B
SB
R�B
UB
VB
UgB
U�B
X_B
X�B
X_B
YB
X�B
X�B
Y�B
Y�B
Y�B
ZQB
[#B
Z�B
Z�B
Z�B
Z�B
[	B
[WB
[WB
[�B
[=B
[�B
[�B
[�B
[�B
\B
\)B
]/B
\�B
\�B
\�B
\�B
]IB
]�B
]dB
]B
\]B
]�B
_;B
_�B
`vB
aHB
aHB
a-B
abB
aHB
a|B
a�B
a-B
a-B
`�B
`\B
`'B
`'B
`�B
abB
bhB
bhB
bhB
b�B
b�B
b�B
c:B
c�B
c�B
c�B
d@B
dtB
d�B
e,B
e�B
e�B
ffB
ffB
f�B
f�B
f�B
gB
gB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h>B
h
B
h
B
h$B
hXB
h�B
i*B
jeB
jB
jeB
jB
jB
j�B
kB
kB
kQB
k�B
kkB
k�B
k�B
lWB
l�B
l�B
l�B
m�B
nB
nIB
ncB
n/B
nB
ncB
ncB
n�B
n}B
n�B
oB
o5B
o5B
oB
oB
o5B
oB
oB
oOB
pUB
p�B
p�B
qB
qAB
q'B
qB
qB
qAB
qvB
qvB
qAB
qAB
qAB
q�B
q�B
q�B
q�B
rGB
r-B
raB
r�B
shB
s�B
s�B
tB
tB
tB
t�B
tTB
tTB
tTB
tTB
tnB
tnB
t�B
uB
u%B
u�B
u�B
v+B
vB
u�B
vFB
u�B
u�B
u�B
u�B
u�B
u�B
v+B
vB
vFB
v�B
v�B
v�B
v�B
v�B
v�B
wB
w�B
w�B
w�B
w�B
x�B
y$B
y�B
y�B
z�B
z�B
{0B
{JB
{�B
{�B
{�B
{�B
|B
|B
|jB
|PB
|PB
|�B
}"B
}VB
}�B
~]B
~�B
~�B
B
.B
�B
�B
�B
�B
�B
� B
�B
�OB
�OB
� B
��B
��B
��B
��B
��B
��B
�oB
�UB
�UB
��B
��B
�AB
�uB
�[B
�uB
�uB
��B
��B
�B
�{B
�{B
�{B
��B
��B
��B
��B
�B
�SB
��B
�B
��B
�%B
�YB
��B
��B
��B
�B
�zB
�_B
�EB
�+B
�EB
��B
�1B
�fB
�fB
��B
��B
��B
��B
��B
�B
�lB
�lB
��B
��B
��B
��B
��B
��B
�#B
�rB
�rB
��B
��B
��B
�xB
�xB
��B
��B
��B
�B
�B
�B
�B
�JB
�dB
�dB
�PB
��B
��B
��B
��B
��B
��B
�<B
�pB
��B
�pB
��B
��B
�(B
�BB
�\B
��B
��B
��B
��B
��B
�HB
�bB
��B
��B
��B
�B
� B
�4B
�hB
��B
��B
��B
��B
��B
�B
� B
�:B
� B
��B
��B
��B
�@B
��B
�B
�FB
�aB
�{B
��B
��B
��B
�B
�B
�MB
��B
�B
�B
�B
�B
�9B
�SB
��B
��B
��B
��B
��B
��B
�?B
�YB
�sB
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230424034816  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230424034828  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230424034828  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230424034829                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230424034829  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230424034829  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230424040720                      G�O�G�O�G�O�                