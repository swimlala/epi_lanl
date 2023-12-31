CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:55:19Z creation;2022-06-04T17:55:19Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604175519  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               <A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�;C��1   @�;C_#E@/KƧ�cA�7K�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bj  Bp  BvffB�  B�  B�ffB�33B�  B���B���B�33B�33B�ffB�  B�ffB�33B�  B�ffB���B�33B�  B�  B̙�B�ffB�ffB�33B�33B�  B㙚B�ffB�33B�  B�B�ffB���C �C�CffC��C��C
33C��C�3C��CL�C�3C��C  C��C�3C� C��C"33C$�C%� C(  C*�C,  C.� C/��C2ffC3��C6�C833C:�C;��C>�C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�C3Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@@r�\@�G�@�G�A��A>=qA\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bi(�Bo(�Bu�\B(�B��{B���B�ǮB��{B�aHB�.B�ǮB�ǮB���B��{B���B�ǮB��{B���B�.B�ǮBÔ{BƔ{B�.B���B���B�ǮB�ǮB��{B�.B���B�ǮB�{B�.B���B�aHB�ǯC��C0�Cc�C�
C	�pC�
C}pC�
C
C}pC�
C�=Cc�C}pCJ=C�
C!�pC#��C%J=C'�=C)��C+�=C.J=C/c�C20�C3c�C5��C7�pC9��C;c�C=��C?�=CA��CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci��Ck�=Cm�=Co��Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D��D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�Dur�Du�Dvr�Dv�Dwr�Dw�Dxr�Dx�Dyr�Dy�Dzr�Dz�D{r�D{�D|r�D|�D}r�D}�D~r�D~�Dr�D�D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�vD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�<{D�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��{D��{D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD¹HD��HD�9HD�yHDùHD��HD�9HD�yHDĹHD��HD�9HD�yHDŹHD��HD�9HD�yHDƹHD��HD�<{D�yHDǹHD��HD�9HD�yHDȹHD��HD�9HD�yHDɹHD��HD�9HD�yHDʹHD��HD�9HD�yHD˹HD��HD�9HD�yHD̹HD��HD�9HD�yHD͹HD��HD�9HD�yHDιHD��HD�9HD�yHDϹHD��HD�9HD�yHDйHD��HD�9HD�yHDѹHD��HD�9HD�yHDҹHD��HD�9HD�yHDӹHD��HD�9HD�yHDԹHD��HD�9HD�yHDչHD��HD�9HD�yHDֹHD��HD�9HD�yHD׹HD��HD�9HD�yHDعHD��HD�9HD�yHDٹHD��HD�9HD�yHDڹHD��HD�9HD�yHD۹HD��HD�9HD�yHDܹHD��HD�9HD�yHDݹHD��HD�9HD�yHD޹HD��HD�9HD�yHD߹HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�D��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��D��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD�߮111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aۨ�AۧRA۱�Aۣ�AۤtA�X�A�CA�_A��xA��A��A���A��2A��A��A�ޞA��]A��)A��WA��A��yA�خA��A��A���A���A��BA��vA�XyA�h�A�=qA���AȢ�AƝ�A�)�A�J�A���A�_A��2A�.A��sA���A�ʌA�bA��8A��A�@�A�T�A�,A��FA��A�oA�.�A��cA�k�A�,�A��A��<A�>�A�*0A��rA�iyA�<jA��A��GA��2A���A��aA�Q�A�W�A�B�A�A�?}A�d�A��A���A���A��XA��nA�2aA�v�A�A�]/A��A�u�A���A�A{xAs��Ar�Ap��Ap��Ak�:Ajj�Ag��Ae�AA`k�A\�AT4ASE9AQ��AN��AJ��AE33AC�ABY�A@_�A?�{A>OA:t�A9�A7{�A6bA4�YA3(�A2��A2l"A2jA4OvA1�A-$�A)1A'b�A&�A%m]A%��A%�~A#�zA"�NA!��A!z�A ��AVA�Au%AQ�AO�AkQAqA�zA�jAߤA�qAU2AD�A�~A�WA��AcAp;Am�A��AsAɆA
=A-�A�A�9AϫA��AVA��AS�A�MA�A \ATaA/A�A�A��A
 iA	jA	A�A��A��A�AxlA�A��A��Ax�A@�A�A��Ai�A��A(A�4A��Ao A�zA��Af�A4A�A��A2�A �	A �A ��A �:A 8@��{@�D�@�U�@��A@�@�6@��E@��o@�h�@�	@�{J@��Y@���@�&�@�?�@�@�,�@�Y@���@��@�@��@�ff@�'R@��#@��@��8@��Z@��6@�t�@��@��@��@蠐@�O@�K�@��@�U2@���@�x@��@�C-@�f�@��s@�E�@�2a@���@�h
@�ݘ@�[W@��s@ތ�@ݠ'@���@�J�@���@ۋ�@�[W@ڔF@�!-@��E@إz@�?�@�ݘ@ח�@�G�@�Ĝ@ե�@�@O@�&�@���@�\�@ӧ�@��@�o @Ќ@�W?@ͽ�@�@��
@͌~@͜�@�iD@�,�@�PH@��@ʎ�@��@�H�@�~�@�i�@��@�A @�)_@�33@ƞ�@�	�@Ţ�@�o�@ŗ�@�dZ@��H@Č@�C-@ÖS@�5�@���@�*�@�S&@��@��U@���@��@��b@��@���@��t@�Vm@�!�@��@�
=@��@��$@�3�@��@��w@�c�@�+@��`@���@��3@�`B@��5@���@�M@�� @�qv@�(@���@�H�@��@��F@���@�j@�=�@��@�$@��@�-w@���@���@�g8@�ƨ@�1�@��X@�1�@��Q@��^@��"@�hs@�2a@��@�[�@��3@�iD@�G�@�33@�o@�ȴ@���@�R�@���@��@��4@�G�@�	l@���@���@�m�@���@�F�@�@O@��@��5@���@�s�@�:�@��@��[@�c�@��y@��@�bN@��@���@��@�J�@�ߤ@�z@�?�@���@��@�g�@�@���@��P@�f�@��9@�1'@��@�m]@�)_@��9@�q@�($@��.@��}@��6@��a@��@�5�@��@���@��.@�t�@��@��f@��,@��@�.�@�ԕ@��@���@���@�[W@�@O@�0�@���@�w�@�0U@�	@��a@�l�@��@��@��m@��@�tT@�4n@��@��{@�C�@��@��@�l"@�J�@�@��T@�g�@�&�@��	@��B@���@���@��}@�xl@�L0@��@��M@�RT@�&@���@��@�L0@�{@��]@��{@�6z@���@�q�@�Q@��r@��z@��9@���@��@��@��O@��@�?@���@��d@��4@�Y�@�2a@�-w@�#�@��@��@���@��1@�oi@��@��@���@�k�@�<6@��|@�ߤ@��@�1�@��@��@��4@�K�@�(�@�
=@��@�ȴ@�tT@�"h@�b@��@��@��=@�n/@�f�@�F�@��@��8@��@���@���@�j@�0U@�O@��o@���@��g@��N@���@�a�@�F�@�1�@���@���@�_@�GE@�6@��j@��"@�K�@�o@�֡@���@�w�@�.�@� �@��#@��
@���@�~�@�Mj@�O@�,�@���@��E@���@�K^@�E�@�;�@��@~�!@}��@}A @}+�@}@|�@{�q@{t�@{U�@z�@zv�@z8�@y�@yk�@x��@xz�@xx@w�V@w>�@v��@v#:@u�n@u-w@t�$@t�@t4n@s��@sC�@r�@r��@r)�@q�@q�=@q%@pĜ@p�_@p(�@o��@o�@n��@n4@m�3@mf�@m�@l֡@l��@ll"@lI�@k��@k�@j}V@j?@i�3@ic�@i7L@i&�@h�U@h�@g��@g'�@fh
@ec�@e�@eq@d�u@d4n@c�F@c@bv�@a�d@a`B@a�@`�@`bN@_ݘ@_�a@_�$@_,�@^��@^�L@^@]�@]N<@]Dg@] \@\��@\��@\2�@[��@[ i@Z�@Z�@Y��@Yzx@Y^�@Y-w@X��@X_@XQ�@XM@X �@W�@@W�@V��@V	@U\�@T|�@S�r@S��@S��@R͟@R��@R)�@Q��@Q0�@P�@P��@O��@O�@O1�@O�@N�X@N��@M�@M�@Mk�@M�@L��@L*�@K�@K��@K�	@KW?@J�@JM�@J	@I�-@Ie,@I2a@H�)@G��@G��@Gg�@G@F�'@F�r@F1�@Eϫ@Ej@E:�@E�@D��@D�@D��@D��@DV�@C��@C+@B��@B�6@Bv�@B1�@B_@A�X@AT�@@��@@?�@@�@?�w@?�4@?6z@>�2@>��@>c @>8�@>�@>�@=�#@=�@=�X@=S&@=�@<�p@<�u@<[�@<'R@;��@;��@;;d@:��@:6�@9��@9�C@9x�@9A @8�`@8Z@8"h@7ݘ@7��@7�@7qv@7H�@7C@7@6��@6:*@5��@5��@5�C@5hs@55�@5V@4�[@4��@4A�@3�@3��@3�@3RT@2��@2q�@2O@24@1�@1�@1��@1<6@1�@0��@0��@0|�@0N�@09X@/�
@/E9@.��@/�@.�'@.#:@-ԕ@-Dg@-	l@,�K@,�U@,D�@+�w@+b�@+�@*�b@*R�@)�@)}�@)m]@)`B@)A @)q@)@(�|@(��@(��@(_@(�@'�q@'o�@'+@&�s@&�b@&L0@&�@&�@&u@%�@%��@%�@%�X@%e,@$��@$�u@$�@#� @#��@#�@"c @"@!�@!?}@ �@ ��@ 'R@ G@�@��@U�@�@��@�+@YK@ �@��@G�@7L@�5@��@��@tT@Z@tT@bN@<�@�]@�k@S�@Y@�y@�m@0U@�@��@m]@%@�E@�z@�@V�@1'@$@x@�@��@��@J#@��@� @q�@@�@�@��@J�@��@�|@��@%�@	�@�g@�@�@iD@/�@�@�s@�F@a|@0U@@��@��@w2@j@%F@��@��@tT@w�@q@>B@�r@�@@��@iD@4�@��@��@�}@�!@�6@~�@	@�>@�#@�3@�n@�=@��@c�@#�@�@�`@��@M@��@��@t�@6z@C@
�"@
�@
��@
��@
�F@
ff@
a|@
C�@
-@
!�@
!�@
	@
_@	�)@	��@	`B@	0�@	q@�@��@Xy@,=@!@�+@��@l�@F�@.I@�@��@�,@�x@{�@ff@Ov@($@J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aۨ�AۧRA۱�Aۣ�AۤtA�X�A�CA�_A��xA��A��A���A��2A��A��A�ޞA��]A��)A��WA��A��yA�خA��A��A���A���A��BA��vA�XyA�h�A�=qA���AȢ�AƝ�A�)�A�J�A���A�_A��2A�.A��sA���A�ʌA�bA��8A��A�@�A�T�A�,A��FA��A�oA�.�A��cA�k�A�,�A��A��<A�>�A�*0A��rA�iyA�<jA��A��GA��2A���A��aA�Q�A�W�A�B�A�A�?}A�d�A��A���A���A��XA��nA�2aA�v�A�A�]/A��A�u�A���A�A{xAs��Ar�Ap��Ap��Ak�:Ajj�Ag��Ae�AA`k�A\�AT4ASE9AQ��AN��AJ��AE33AC�ABY�A@_�A?�{A>OA:t�A9�A7{�A6bA4�YA3(�A2��A2l"A2jA4OvA1�A-$�A)1A'b�A&�A%m]A%��A%�~A#�zA"�NA!��A!z�A ��AVA�Au%AQ�AO�AkQAqA�zA�jAߤA�qAU2AD�A�~A�WA��AcAp;Am�A��AsAɆA
=A-�A�A�9AϫA��AVA��AS�A�MA�A \ATaA/A�A�A��A
 iA	jA	A�A��A��A�AxlA�A��A��Ax�A@�A�A��Ai�A��A(A�4A��Ao A�zA��Af�A4A�A��A2�A �	A �A ��A �:A 8@��{@�D�@�U�@��A@�@�6@��E@��o@�h�@�	@�{J@��Y@���@�&�@�?�@�@�,�@�Y@���@��@�@��@�ff@�'R@��#@��@��8@��Z@��6@�t�@��@��@��@蠐@�O@�K�@��@�U2@���@�x@��@�C-@�f�@��s@�E�@�2a@���@�h
@�ݘ@�[W@��s@ތ�@ݠ'@���@�J�@���@ۋ�@�[W@ڔF@�!-@��E@إz@�?�@�ݘ@ח�@�G�@�Ĝ@ե�@�@O@�&�@���@�\�@ӧ�@��@�o @Ќ@�W?@ͽ�@�@��
@͌~@͜�@�iD@�,�@�PH@��@ʎ�@��@�H�@�~�@�i�@��@�A @�)_@�33@ƞ�@�	�@Ţ�@�o�@ŗ�@�dZ@��H@Č@�C-@ÖS@�5�@���@�*�@�S&@��@��U@���@��@��b@��@���@��t@�Vm@�!�@��@�
=@��@��$@�3�@��@��w@�c�@�+@��`@���@��3@�`B@��5@���@�M@�� @�qv@�(@���@�H�@��@��F@���@�j@�=�@��@�$@��@�-w@���@���@�g8@�ƨ@�1�@��X@�1�@��Q@��^@��"@�hs@�2a@��@�[�@��3@�iD@�G�@�33@�o@�ȴ@���@�R�@���@��@��4@�G�@�	l@���@���@�m�@���@�F�@�@O@��@��5@���@�s�@�:�@��@��[@�c�@��y@��@�bN@��@���@��@�J�@�ߤ@�z@�?�@���@��@�g�@�@���@��P@�f�@��9@�1'@��@�m]@�)_@��9@�q@�($@��.@��}@��6@��a@��@�5�@��@���@��.@�t�@��@��f@��,@��@�.�@�ԕ@��@���@���@�[W@�@O@�0�@���@�w�@�0U@�	@��a@�l�@��@��@��m@��@�tT@�4n@��@��{@�C�@��@��@�l"@�J�@�@��T@�g�@�&�@��	@��B@���@���@��}@�xl@�L0@��@��M@�RT@�&@���@��@�L0@�{@��]@��{@�6z@���@�q�@�Q@��r@��z@��9@���@��@��@��O@��@�?@���@��d@��4@�Y�@�2a@�-w@�#�@��@��@���@��1@�oi@��@��@���@�k�@�<6@��|@�ߤ@��@�1�@��@��@��4@�K�@�(�@�
=@��@�ȴ@�tT@�"h@�b@��@��@��=@�n/@�f�@�F�@��@��8@��@���@���@�j@�0U@�O@��o@���@��g@��N@���@�a�@�F�@�1�@���@���@�_@�GE@�6@��j@��"@�K�@�o@�֡@���@�w�@�.�@� �@��#@��
@���@�~�@�Mj@�O@�,�@���@��E@���@�K^@�E�@�;�@��@~�!@}��@}A @}+�@}@|�@{�q@{t�@{U�@z�@zv�@z8�@y�@yk�@x��@xz�@xx@w�V@w>�@v��@v#:@u�n@u-w@t�$@t�@t4n@s��@sC�@r�@r��@r)�@q�@q�=@q%@pĜ@p�_@p(�@o��@o�@n��@n4@m�3@mf�@m�@l֡@l��@ll"@lI�@k��@k�@j}V@j?@i�3@ic�@i7L@i&�@h�U@h�@g��@g'�@fh
@ec�@e�@eq@d�u@d4n@c�F@c@bv�@a�d@a`B@a�@`�@`bN@_ݘ@_�a@_�$@_,�@^��@^�L@^@]�@]N<@]Dg@] \@\��@\��@\2�@[��@[ i@Z�@Z�@Y��@Yzx@Y^�@Y-w@X��@X_@XQ�@XM@X �@W�@@W�@V��@V	@U\�@T|�@S�r@S��@S��@R͟@R��@R)�@Q��@Q0�@P�@P��@O��@O�@O1�@O�@N�X@N��@M�@M�@Mk�@M�@L��@L*�@K�@K��@K�	@KW?@J�@JM�@J	@I�-@Ie,@I2a@H�)@G��@G��@Gg�@G@F�'@F�r@F1�@Eϫ@Ej@E:�@E�@D��@D�@D��@D��@DV�@C��@C+@B��@B�6@Bv�@B1�@B_@A�X@AT�@@��@@?�@@�@?�w@?�4@?6z@>�2@>��@>c @>8�@>�@>�@=�#@=�@=�X@=S&@=�@<�p@<�u@<[�@<'R@;��@;��@;;d@:��@:6�@9��@9�C@9x�@9A @8�`@8Z@8"h@7ݘ@7��@7�@7qv@7H�@7C@7@6��@6:*@5��@5��@5�C@5hs@55�@5V@4�[@4��@4A�@3�@3��@3�@3RT@2��@2q�@2O@24@1�@1�@1��@1<6@1�@0��@0��@0|�@0N�@09X@/�
@/E9@.��@/�@.�'@.#:@-ԕ@-Dg@-	l@,�K@,�U@,D�@+�w@+b�@+�@*�b@*R�@)�@)}�@)m]@)`B@)A @)q@)@(�|@(��@(��@(_@(�@'�q@'o�@'+@&�s@&�b@&L0@&�@&�@&u@%�@%��@%�@%�X@%e,@$��@$�u@$�@#� @#��@#�@"c @"@!�@!?}@ �@ ��@ 'R@ G@�@��@U�@�@��@�+@YK@ �@��@G�@7L@�5@��@��@tT@Z@tT@bN@<�@�]@�k@S�@Y@�y@�m@0U@�@��@m]@%@�E@�z@�@V�@1'@$@x@�@��@��@J#@��@� @q�@@�@�@��@J�@��@�|@��@%�@	�@�g@�@�@iD@/�@�@�s@�F@a|@0U@@��@��@w2@j@%F@��@��@tT@w�@q@>B@�r@�@@��@iD@4�@��@��@�}@�!@�6@~�@	@�>@�#@�3@�n@�=@��@c�@#�@�@�`@��@M@��@��@t�@6z@C@
�"@
�@
��@
��@
�F@
ff@
a|@
C�@
-@
!�@
!�@
	@
_@	�)@	��@	`B@	0�@	q@�@��@Xy@,=@!@�+@��@l�@F�@.I@�@��@�,@�x@{�@ff@Ov@($@J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B��B�B�B�B�B�eB�B�B�B�B�B�B�B�B�B��B�6B�kB�B�B�B�WB�B�B��B�!B	BňB�<B	��B	�B	ǮB	�kB	�nB	�B	�dB	�B	��B	��B	��B	�B
~B
#:B
8B
=<B
@�B
H1B
R�B
h�B
|B
��B
�7B
��B
�wB
��B
�uB
�;B
�yB
�B#TBF�BC�B-�B>]BF�BbNBt�B��B�9B�5B��B��Bd�B4�B$@B
��B
�IB
��B
�kB
HB
^B
&�B
_B	�{B	��B	r�B	O\B	I�B	G�B	TB	V9B	I7B	<�B	4nB	!�B	B�B�B	 �B	�B	
�B��B�B��B�B�eB�8B��B��B��B��B�pBچB�IB�B�}B	C�B	:�B	$�B	�B�lB��B�FB	VB	+kB	#�B	�B	�B	0�B	:�B	6`B	KxB	K�B	F�B	C�B	?cB	AB	K^B	XEB	[�B	W�B	\CB	ezB	qAB	��B	��B	��B	��B	��B	��B	�,B	� B	�)B	�B	�zB	��B	�XB	��B	�GB	�[B	�B	��B	��B	��B	�fB	��B	��B	�OB	ðB	��B	��B	��B	��B	�fB	�AB	�B	��B	�%B	�#B	��B	ΊB	�PB	�0B	өB	�B	��B	�4B	�B	��B	��B	ˬB	͹B	�jB	̳B	�DB	�rB	��B	��B	�(B	�4B	ѷB	бB	�"B	�=B	�	B	��B	��B	�+B	�=B	ȀB	�1B	�B	ʌB	�B	�B	�PB	�0B	�B	��B	��B	�6B	�<B	ΊB	�VB	��B	̈́B	̳B	�B	��B	ǮB	ǮB	ɆB	ʌB	�lB	ʦB	�pB	�B	�B	˒B	��B	�B	̘B	�"B	��B	��B	��B	�B	��B	ּB	��B	�EB	�?B	�9B	��B	�B	��B	ևB	�9B	�9B	�B	��B	�?B	ևB	�B	յB	�B	�B	�B	ּB	�B	ٴB	ٴB	�1B	�B	خB	�
B	�?B	յB	��B	��B	�4B	�,B	��B	�$B	�
B	��B	�YB	�?B	רB	׍B	�?B	��B	�gB	�aB	�[B	��B	��B	�mB	�B	��B	��B	��B	�B	��B	�7B	ݲB	ބB	�B	��B	ܬB	�=B	�B	�IB	�-B	�B	�`B	�B	�B	��B	�B	�B	�B	�eB	��B	�)B	��B	��B	��B	�'B	�vB	�B	��B	��B	�GB	�GB	�B	�B	�MB	�MB	�B	�TB	��B	��B	�B	��B	�+B	��B	��B	�+B	��B	�2B	��B	�8B	�>B	��B	�>B	�	B	�>B	�$B	�	B	�	B	�	B	��B	��B	��B	��B	�B	�B	��B	�B	��B	�VB	�<B	�"B	��B	��B	�B	�qB	��B	��B	��B	�wB	��B	�BB	��B	��B	��B	�.B	�.B	�}B	�cB	��B
 OB
 4B
 �B
 �B
 �B
 �B
B
oB
'B
AB
�B
�B
B
B
B
 �B
 �B
�B
AB
B
B
-B
B
3B
�B
�B
�B
�B
�B
�B
mB
mB
�B
�B
�B
	B
	lB

�B

�B
�B
JB
�B
�B
�B
�B
�B
�B
�B
�B
PB
�B
B
PB
�B
pB
pB
�B
�B
�B
�B
�B
BB
�B
.B
bB
bB
}B
}B
hB
hB
�B
�B
�B
 B
�B
�B
�B
B
�B
�B
�B
�B
�B
{B
gB
�B

B
�B
�B
�B
mB
�B
�B
�B
�B
B
B
kB
�B
#B
=B
�B
)B
]B
�B
�B
�B
B
~B
�B
�B
�B
�B
�B
�B
B
�B
 B
�B
 BB
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 vB
 �B
 vB
 �B
!-B
!�B
!�B
!�B
!�B
"�B
#TB
#nB
#nB
$B
$@B
$�B
%,B
%FB
%�B
&B
%�B
%�B
%�B
&fB
&fB
&LB
&�B
'B
'8B
&�B
&�B
'B
'�B
(XB
)B
)yB
)yB
)�B
)DB
)DB
)*B
)*B
)B
)*B
(�B
)�B
*�B
+B
*�B
+B
+B
+kB
,qB
,�B
,B
+B
+�B
,�B
.B
-�B
-CB
-]B
-]B
-�B
.IB
0UB
0�B
1vB
1�B
1�B
1�B
1�B
2-B
2aB
33B
3�B
3�B
3�B
4B
4nB
4nB
4�B
4�B
4�B
4�B
4�B
5?B
5�B
5�B
6B
5�B
5�B
5�B
5�B
5�B
6FB
6�B
6�B
7B
7�B
7�B
8lB
9$B
9�B
9�B
9rB
9rB
9XB
9>B
9rB
9�B
9�B
9rB
9rB
9�B
9�B
:�B
;JB
<B
;�B
<B
;�B
<6B
<�B
<�B
="B
>(B
=�B
>(B
>�B
?cB
?�B
?�B
?�B
?�B
?�B
?�B
?�B
@ B
@iB
AB
A B
A�B
B'B
B�B
C-B
C-B
C�B
C�B
C�B
DB
DMB
DgB
DMB
DMB
D�B
EmB
ESB
D�B
DMB
EB
ESB
ESB
E�B
FB
E�B
E�B
FYB
F�B
F�B
F�B
G+B
G_B
HB
HB
HKB
H1B
H�B
I7B
I7B
I�B
I�B
J#B
J#B
J=B
JrB
J�B
KB
K^B
KxB
K�B
K�B
K�B
LB
MB
MPB
MjB
M�B
M�B
M�B
M�B
NB
NVB
NpB
N�B
N�B
O(B
O�B
P�B
Q4B
Q�B
RB
Q�B
RB
R B
RB
Q�B
RTB
R�B
R�B
R�B
R�B
S&B
S�B
SuB
S�B
S�B
S�B
S�B
T,B
T�B
T�B
T�B
UB
U�B
UgB
VmB
V�B
W
B
WYB
W�B
W�B
XB
XEB
X_B
XyB
Y1B
X�B
X�B
Y1B
Y�B
YB
Y�B
Y�B
Z�B
[#B
[WB
[qB
[qB
[�B
\CB
\CB
\)B
\�B
\�B
\�B
\�B
]B
\�B
\�B
\�B
\xB
\�B
\�B
]B
]~B
]�B
]�B
]�B
]�B
^B
^OB
^OB
^�B
_!B
_;B
_VB
`B
`'B
`\B
`\B
`'B
`�B
aHB
a�B
b4B
a�B
a�B
a�B
a�B
b�B
b�B
c B
c�B
c�B
d@B
d�B
d�B
eB
e,B
eFB
e`B
ezB
e�B
ezB
e�B
e�B
fB
fB
f�B
f�B
gB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h$B
iDB
iB
h�B
h�B
i_B
i�B
i_B
i�B
i�B
j0B
j�B
kB
kkB
kQB
kkB
lB
l=B
l=B
l"B
lqB
l�B
l�B
m]B
mwB
mwB
m�B
nIB
n�B
n�B
o5B
pUB
p;B
p;B
p!B
pB
p�B
qB
q[B
qvB
rB
q�B
rGB
r|B
r�B
raB
raB
r�B
r�B
r�B
s3B
shB
s�B
s�B
tB
tnB
t�B
tnB
tTB
t�B
t�B
t�B
t�B
t�B
uB
u�B
v+B
vzB
v�B
v�B
w2B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xlB
x�B
x�B
y$B
y>B
y�B
zB
z^B
z^B
zxB
z^B
z�B
z�B
{dB
{B
{B
{�B
|B
|B
|B
|B
|B
|6B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}"B
}<B
}�B
~B
~]B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
.B
cB
cB
�B
�iB
��B
��B
��B
�B
�UB
�oB
�oB
��B
��B
�'B
��B
��B
�B
�'B
�[B
�uB
��B
��B
��B
�B
�B
�-B
�aB
�{B
��B
�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B��B�B�B�B�B�eB�B�B�B�B�B�B�B�B�B��B�6B�kB�B�B�B�WB�B�B��B�!B	BňB�<B	��B	�B	ǮB	�kB	�nB	�B	�dB	�B	��B	��B	��B	�B
~B
#:B
8B
=<B
@�B
H1B
R�B
h�B
|B
��B
�7B
��B
�wB
��B
�uB
�;B
�yB
�B#TBF�BC�B-�B>]BF�BbNBt�B��B�9B�5B��B��Bd�B4�B$@B
��B
�IB
��B
�kB
HB
^B
&�B
_B	�{B	��B	r�B	O\B	I�B	G�B	TB	V9B	I7B	<�B	4nB	!�B	B�B�B	 �B	�B	
�B��B�B��B�B�eB�8B��B��B��B��B�pBچB�IB�B�}B	C�B	:�B	$�B	�B�lB��B�FB	VB	+kB	#�B	�B	�B	0�B	:�B	6`B	KxB	K�B	F�B	C�B	?cB	AB	K^B	XEB	[�B	W�B	\CB	ezB	qAB	��B	��B	��B	��B	��B	��B	�,B	� B	�)B	�B	�zB	��B	�XB	��B	�GB	�[B	�B	��B	��B	��B	�fB	��B	��B	�OB	ðB	��B	��B	��B	��B	�fB	�AB	�B	��B	�%B	�#B	��B	ΊB	�PB	�0B	өB	�B	��B	�4B	�B	��B	��B	ˬB	͹B	�jB	̳B	�DB	�rB	��B	��B	�(B	�4B	ѷB	бB	�"B	�=B	�	B	��B	��B	�+B	�=B	ȀB	�1B	�B	ʌB	�B	�B	�PB	�0B	�B	��B	��B	�6B	�<B	ΊB	�VB	��B	̈́B	̳B	�B	��B	ǮB	ǮB	ɆB	ʌB	�lB	ʦB	�pB	�B	�B	˒B	��B	�B	̘B	�"B	��B	��B	��B	�B	��B	ּB	��B	�EB	�?B	�9B	��B	�B	��B	ևB	�9B	�9B	�B	��B	�?B	ևB	�B	յB	�B	�B	�B	ּB	�B	ٴB	ٴB	�1B	�B	خB	�
B	�?B	յB	��B	��B	�4B	�,B	��B	�$B	�
B	��B	�YB	�?B	רB	׍B	�?B	��B	�gB	�aB	�[B	��B	��B	�mB	�B	��B	��B	��B	�B	��B	�7B	ݲB	ބB	�B	��B	ܬB	�=B	�B	�IB	�-B	�B	�`B	�B	�B	��B	�B	�B	�B	�eB	��B	�)B	��B	��B	��B	�'B	�vB	�B	��B	��B	�GB	�GB	�B	�B	�MB	�MB	�B	�TB	��B	��B	�B	��B	�+B	��B	��B	�+B	��B	�2B	��B	�8B	�>B	��B	�>B	�	B	�>B	�$B	�	B	�	B	�	B	��B	��B	��B	��B	�B	�B	��B	�B	��B	�VB	�<B	�"B	��B	��B	�B	�qB	��B	��B	��B	�wB	��B	�BB	��B	��B	��B	�.B	�.B	�}B	�cB	��B
 OB
 4B
 �B
 �B
 �B
 �B
B
oB
'B
AB
�B
�B
B
B
B
 �B
 �B
�B
AB
B
B
-B
B
3B
�B
�B
�B
�B
�B
�B
mB
mB
�B
�B
�B
	B
	lB

�B

�B
�B
JB
�B
�B
�B
�B
�B
�B
�B
�B
PB
�B
B
PB
�B
pB
pB
�B
�B
�B
�B
�B
BB
�B
.B
bB
bB
}B
}B
hB
hB
�B
�B
�B
 B
�B
�B
�B
B
�B
�B
�B
�B
�B
{B
gB
�B

B
�B
�B
�B
mB
�B
�B
�B
�B
B
B
kB
�B
#B
=B
�B
)B
]B
�B
�B
�B
B
~B
�B
�B
�B
�B
�B
�B
B
�B
 B
�B
 BB
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 vB
 �B
 vB
 �B
!-B
!�B
!�B
!�B
!�B
"�B
#TB
#nB
#nB
$B
$@B
$�B
%,B
%FB
%�B
&B
%�B
%�B
%�B
&fB
&fB
&LB
&�B
'B
'8B
&�B
&�B
'B
'�B
(XB
)B
)yB
)yB
)�B
)DB
)DB
)*B
)*B
)B
)*B
(�B
)�B
*�B
+B
*�B
+B
+B
+kB
,qB
,�B
,B
+B
+�B
,�B
.B
-�B
-CB
-]B
-]B
-�B
.IB
0UB
0�B
1vB
1�B
1�B
1�B
1�B
2-B
2aB
33B
3�B
3�B
3�B
4B
4nB
4nB
4�B
4�B
4�B
4�B
4�B
5?B
5�B
5�B
6B
5�B
5�B
5�B
5�B
5�B
6FB
6�B
6�B
7B
7�B
7�B
8lB
9$B
9�B
9�B
9rB
9rB
9XB
9>B
9rB
9�B
9�B
9rB
9rB
9�B
9�B
:�B
;JB
<B
;�B
<B
;�B
<6B
<�B
<�B
="B
>(B
=�B
>(B
>�B
?cB
?�B
?�B
?�B
?�B
?�B
?�B
?�B
@ B
@iB
AB
A B
A�B
B'B
B�B
C-B
C-B
C�B
C�B
C�B
DB
DMB
DgB
DMB
DMB
D�B
EmB
ESB
D�B
DMB
EB
ESB
ESB
E�B
FB
E�B
E�B
FYB
F�B
F�B
F�B
G+B
G_B
HB
HB
HKB
H1B
H�B
I7B
I7B
I�B
I�B
J#B
J#B
J=B
JrB
J�B
KB
K^B
KxB
K�B
K�B
K�B
LB
MB
MPB
MjB
M�B
M�B
M�B
M�B
NB
NVB
NpB
N�B
N�B
O(B
O�B
P�B
Q4B
Q�B
RB
Q�B
RB
R B
RB
Q�B
RTB
R�B
R�B
R�B
R�B
S&B
S�B
SuB
S�B
S�B
S�B
S�B
T,B
T�B
T�B
T�B
UB
U�B
UgB
VmB
V�B
W
B
WYB
W�B
W�B
XB
XEB
X_B
XyB
Y1B
X�B
X�B
Y1B
Y�B
YB
Y�B
Y�B
Z�B
[#B
[WB
[qB
[qB
[�B
\CB
\CB
\)B
\�B
\�B
\�B
\�B
]B
\�B
\�B
\�B
\xB
\�B
\�B
]B
]~B
]�B
]�B
]�B
]�B
^B
^OB
^OB
^�B
_!B
_;B
_VB
`B
`'B
`\B
`\B
`'B
`�B
aHB
a�B
b4B
a�B
a�B
a�B
a�B
b�B
b�B
c B
c�B
c�B
d@B
d�B
d�B
eB
e,B
eFB
e`B
ezB
e�B
ezB
e�B
e�B
fB
fB
f�B
f�B
gB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h$B
iDB
iB
h�B
h�B
i_B
i�B
i_B
i�B
i�B
j0B
j�B
kB
kkB
kQB
kkB
lB
l=B
l=B
l"B
lqB
l�B
l�B
m]B
mwB
mwB
m�B
nIB
n�B
n�B
o5B
pUB
p;B
p;B
p!B
pB
p�B
qB
q[B
qvB
rB
q�B
rGB
r|B
r�B
raB
raB
r�B
r�B
r�B
s3B
shB
s�B
s�B
tB
tnB
t�B
tnB
tTB
t�B
t�B
t�B
t�B
t�B
uB
u�B
v+B
vzB
v�B
v�B
w2B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xlB
x�B
x�B
y$B
y>B
y�B
zB
z^B
z^B
zxB
z^B
z�B
z�B
{dB
{B
{B
{�B
|B
|B
|B
|B
|B
|6B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}"B
}<B
}�B
~B
~]B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
.B
cB
cB
�B
�iB
��B
��B
��B
�B
�UB
�oB
�oB
��B
��B
�'B
��B
��B
�B
�'B
�[B
�uB
��B
��B
��B
�B
�B
�-B
�aB
�{B
��B
�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104959  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175519  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175519  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175519                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025527  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025527  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                