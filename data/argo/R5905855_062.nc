CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:21:33Z creation;2022-06-04T19:21:33Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192133  20220610151509  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               >A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�AEj�d�1   @�AE��]@,�-�ch1&�y1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A���A�B ffBffB��B  B   B(ffB0��B8  B@ffBG33BO��BW��B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���BÙ�B�  B���B�  B���B�  B�  B�  B�  B�ffB�  B�  B�33B���B���C   C  C  C�fC  C
  C  C  C  C  C�C33C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D���D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @(�@r�\@�G�@�G�A=qA<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A��A��A��B�\BB(�B(�B'�\B/��B7(�B?�\BF\)BNBVB_(�Bg(�Bo(�Bw(�B(�B��{B��{B�aHB��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B�.B�.B�.Bǔ{B�aHBϔ{B�aHBה{B۔{Bߔ{B�{B���B�{B�{B�ǮB�.B�aHB��{C�=C�=C��C�=C	�=C�=C�=C�=C�=C��C�pC�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG��CI��CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dx�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ��DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�Dur�Du�Dvr�Dv�Dwr�Dw�Dxr�Dx�Dyr�Dy�Dzr�Dz�D{r�D{�D|r�D|�D}r�D}�D~r�D~�Dr�D�D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�|{D��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��D�9HD�yHD¹HD��HD�9HD�yHDùHD��HD�9HD�yHDĹHD��HD�9HD�yHDŹHD��D�9HD�yHDƹHD��HD�9HD�yHDǹHD��HD�9HD�yHDȹHD��HD�9HD�yHDɹHD��HD�9HD�yHDʹHD��HD�9HD�yHD˹HD��HD�9HD�yHD̹HD��HD�9HD�yHD͹HD��HD�9HD�yHDιHD��HD�9HD�yHDϹHD��HD�9HD�yHDйHD��HD�9HD�yHDѹHD��HD�9HD�yHDҹHD��HD�9HD�yHDӹHD��HD�9HD�yHDԹHD��HD�9HD�yHDչHD��HD�9HD�yHDֹHD��HD�9HD�yHD׹HD��HD�9HD�yHDعHD��HD�9HD�yHDٹHD��HD�9HD�yHDڹHD��HD�9HD�yHD۹HD��HD�9HD�yHDܹHD��HD�9HD�yHDݹHD��HD�9HD�yHD޹HD��HD�9HD�yHD߹HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��&A�ܒA��AݵtAݓ�AݕMAݑhA�[�A�7�A�0UA�&�A�!�A��A�7A�{A�PA�LdA�=qA�m]AЂ�A�-�Aͩ_A�yrA�e,A�یA�K�AǗ�A��A�K^A��AĐ�A¢hA���A��+A�R A��YA��A�A���A���A��sA�iDA��pA�:^A�<6A�&A���A��FA��vA���A�^�A�iyA��A��jA�3�A�GA�*eA�x�A�dZA��A��zA���A�jA�@A���A�K�A��A�1A��A��A�poA|��Ay^5Aue,AoԕAm!�Ai~�Ag�Ab�CA_�A]XyAYI�AVXAT�`ASSAP1'AM�]AM6AKJAI�:AH�AD��AB��A?QA>`BA=��A<Q�A:�[A9�hA7�dA7Z�A5�^A57LA4�A3s�A1��A1L0A.W?A,�PA*�vA)��A)��A)��A)e,A(1�A'j�A&\)A&�7A&OvA%�oA$�A%qA$֡A#�oA#�A"[�A"�A!cA m�A��A�A	�A�A�A��A�yA��ARTA�A�A�]A�A_A��AqA�!A(A�@A�Au�A��A�A��As�A�AJ#A��A�3Ac�Ar�A
_�A	�A	n/A�3A�A"hAɆA�AW?A�A�zA��AoiA�AA��A5?A A/�Al�A>BA ��A ��A �?A ��A ��A +kA e@��]@���@���@���@�7L@�Q�@��@�C@���@�1�@�'R@�a|@�2a@���@��>@�n/@���@�|@�C@��@���@랄@ꎊ@�6@�{@��@�~(@��@�C@��
@�@�7�@��Q@�8@�p;@�@�@��N@�H@ߒ:@ޫ6@ޡb@ޅ�@��d@�4n@�C-@ׇ�@�{�@��@���@��r@�k�@�	l@Ҍ�@��@���@���@�o @�(@ι$@�:*@���@��@�c @��@�1@�ԕ@�j@ʫ6@�#:@���@�+@Ȫe@�YK@���@�`�@��P@�"�@�@O@�C@���@�ԕ@�b@���@�[W@��,@Ƚ<@��@�(�@�;@�w�@ǰ�@�]�@�qv@�zx@�x@�u�@�?}@�#�@���@�,=@�˒@Š'@�zx@��@�Ɇ@Ě@�~�@�:*@��m@ßV@�?}@�
=@��|@°�@���@�iD@�e�@�U�@���@�u�@�1@���@�#�@��)@�u%@�7@��t@��P@��1@�0U@��@��)@���@��{@�:�@�ѷ@�Ft@��@��y@��]@��<@�:�@��W@��3@���@�B�@��y@� �@���@��{@��@��j@��D@�{@��o@���@�5�@�~@�e,@�(�@��,@�!�@�S&@�q@�C-@�8�@�7@���@��g@�~�@���@���@�p;@�5?@�J@��D@��-@�X@��P@�0U@���@��f@�o�@�W?@��@��e@�_�@�-@���@�X�@��@��)@���@�;�@�˒@�qv@�L�@� \@��|@�|�@�Z�@�3�@���@���@�`B@���@�PH@���@��"@�o�@�^�@�Y@��<@��\@���@���@�p;@���@���@�?}@��`@��@��@��@�k�@��@��R@�V@�O@��@��@�ff@�=q@�{@���@��@��@���@���@�h
@�J�@�~@���@�qv@���@�n�@��@��@��@���@�|�@�7L@��	@��?@�tT@�$�@�{@���@�=@��@���@�Z�@� �@�hs@��B@�r�@�~@��0@��=@�qv@�.I@��@��@��U@�M�@��@��n@�s@�X@�H�@��@���@�Ov@���@��@�a@� i@��E@���@�ں@���@�PH@�($@���@��@�9�@��@��@�}V@�h�@�8�@��@��@�rG@�o@���@��+@�PH@�@��;@��=@�j@�N<@�<6@�#�@��@���@�q@�Ov@���@���@��7@�X@��@��B@��@�a|@�@��@��6@���@��@�T�@��c@�>B@�@�K@�P@�4@K�@~��@~_@}��@}|@}Vm@}@@|~(@|<�@{��@{n/@z�x@z=q@z!�@y�@yu�@x�	@xe�@w�;@w�@v�]@v�<@vv�@u��@u�@tɆ@t:�@s�V@s8@rC�@q��@q��@qN<@p��@p��@pl"@o��@o"�@nZ�@m�^@m^�@lی@lN�@k�a@j�@jW�@j�@i�@i`B@iO�@i5�@iV@h�v@h��@h`�@g��@g1�@f��@fM�@e�D@e��@e�=@eT�@e+@dѷ@dy>@c��@c��@c�@b��@bq�@a��@a�M@a#�@`�v@`��@_�r@_x@_A�@^�B@^�A@]��@]��@]@@\֡@\�D@\b@[�}@[o�@[&@[S@Z�B@Z=q@Z@Y�S@X��@X��@X6@W�;@Wx@W;d@W�@V�@V�L@VGE@V+k@V�@V	@U�=@Up�@U-w@U�@T�$@T�@S��@R�@R#:@Q��@Q�~@Qf�@QIR@QV@P�@P��@PtT@P<�@O��@O�@N��@N�F@N	@M�D@M��@M�@L��@L��@L-�@K˒@K8@J��@Jd�@I��@I��@Ik�@Ia�@IO�@I<6@I-w@I@@H�@H�u@H2�@G�m@G��@G;d@F��@Fȴ@Fff@F �@E��@E^�@Dw�@C��@C��@CX�@C4�@B�@B�L@Bc @B+k@A��@A��@Af�@A!�@@�f@@l"@?�r@?�g@?��@?8@>��@>��@>@=�S@=7L@<�@<�U@<oi@<-�@<  @;��@;]�@;�@:�@:l�@9��@9�@9�z@9��@9�M@9L�@8�P@8��@8Xy@8?�@8�@7��@6�"@6c @6�@5ϫ@5��@5\�@55�@5*0@5%F@5 \@5�@4�@4Ĝ@4e�@3ݘ@3��@3�4@2��@2�@2kQ@24@1�@1S&@0�@0�O@0|�@0/�@/��@/˒@/�:@/Mj@/'�@.��@._�@.C�@-��@-��@-[W@-F@-(�@-	l@,�@,��@,`�@,1@+�}@+��@+'�@*�!@*~�@*p;@*n�@*6�@)�>@)��@)��@)f�@)Dg@);@(�@(`�@(  @'��@'iD@'F�@'&@'�@&��@&�@&~�@&#:@%��@%�"@%k�@$��@$tT@$"h@#�@#�:@#dZ@#9�@"�@"�+@"p;@"W�@"!�@!@ ��@ y>@ Z@ M@ D�@ 6@ �@�6@�0@�f@C�@�@ i@�M@�]@ȴ@��@=q@{@��@�h@8�@%F@��@�U@�@4n@�&@��@>�@�y@~�@R�@:*@4@�D@��@@��@\�@N<@7L@�5@��@e�@?�@>B@�@��@�P@iD@F�@�@�L@�@n�@�@��@�@s�@5�@�@��@�@q@7�@�@� @��@�	@P�@�@�@��@� @@�@�@�@�H@��@}�@j@N<@8�@�@�@�@�@�[@j�@�@�s@��@��@q�@Ta@($@ �@�@��@o @Q�@@�@�@�|@�E@�z@��@��@�.@S�@�@��@�q@|�@l�@a@/�@�@�@�@
��@
p;@
=q@
+k@
4@	��@	�X@	��@	��@	`B@	8�@	;@�p@��@�@m�@bN@K^@ �@�;@ƨ@�0@��@�	@�4@v`@]�@C�@9�@�@�@�@��@�+@a|@_�@E�@
�@ԕ@��@|@\�@&�@�@��@�@�?@�j@��@��@��@��@y>@2�@�@��@\)@K�@1�@�@�@�@�@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��&A�ܒA��AݵtAݓ�AݕMAݑhA�[�A�7�A�0UA�&�A�!�A��A�7A�{A�PA�LdA�=qA�m]AЂ�A�-�Aͩ_A�yrA�e,A�یA�K�AǗ�A��A�K^A��AĐ�A¢hA���A��+A�R A��YA��A�A���A���A��sA�iDA��pA�:^A�<6A�&A���A��FA��vA���A�^�A�iyA��A��jA�3�A�GA�*eA�x�A�dZA��A��zA���A�jA�@A���A�K�A��A�1A��A��A�poA|��Ay^5Aue,AoԕAm!�Ai~�Ag�Ab�CA_�A]XyAYI�AVXAT�`ASSAP1'AM�]AM6AKJAI�:AH�AD��AB��A?QA>`BA=��A<Q�A:�[A9�hA7�dA7Z�A5�^A57LA4�A3s�A1��A1L0A.W?A,�PA*�vA)��A)��A)��A)e,A(1�A'j�A&\)A&�7A&OvA%�oA$�A%qA$֡A#�oA#�A"[�A"�A!cA m�A��A�A	�A�A�A��A�yA��ARTA�A�A�]A�A_A��AqA�!A(A�@A�Au�A��A�A��As�A�AJ#A��A�3Ac�Ar�A
_�A	�A	n/A�3A�A"hAɆA�AW?A�A�zA��AoiA�AA��A5?A A/�Al�A>BA ��A ��A �?A ��A ��A +kA e@��]@���@���@���@�7L@�Q�@��@�C@���@�1�@�'R@�a|@�2a@���@��>@�n/@���@�|@�C@��@���@랄@ꎊ@�6@�{@��@�~(@��@�C@��
@�@�7�@��Q@�8@�p;@�@�@��N@�H@ߒ:@ޫ6@ޡb@ޅ�@��d@�4n@�C-@ׇ�@�{�@��@���@��r@�k�@�	l@Ҍ�@��@���@���@�o @�(@ι$@�:*@���@��@�c @��@�1@�ԕ@�j@ʫ6@�#:@���@�+@Ȫe@�YK@���@�`�@��P@�"�@�@O@�C@���@�ԕ@�b@���@�[W@��,@Ƚ<@��@�(�@�;@�w�@ǰ�@�]�@�qv@�zx@�x@�u�@�?}@�#�@���@�,=@�˒@Š'@�zx@��@�Ɇ@Ě@�~�@�:*@��m@ßV@�?}@�
=@��|@°�@���@�iD@�e�@�U�@���@�u�@�1@���@�#�@��)@�u%@�7@��t@��P@��1@�0U@��@��)@���@��{@�:�@�ѷ@�Ft@��@��y@��]@��<@�:�@��W@��3@���@�B�@��y@� �@���@��{@��@��j@��D@�{@��o@���@�5�@�~@�e,@�(�@��,@�!�@�S&@�q@�C-@�8�@�7@���@��g@�~�@���@���@�p;@�5?@�J@��D@��-@�X@��P@�0U@���@��f@�o�@�W?@��@��e@�_�@�-@���@�X�@��@��)@���@�;�@�˒@�qv@�L�@� \@��|@�|�@�Z�@�3�@���@���@�`B@���@�PH@���@��"@�o�@�^�@�Y@��<@��\@���@���@�p;@���@���@�?}@��`@��@��@��@�k�@��@��R@�V@�O@��@��@�ff@�=q@�{@���@��@��@���@���@�h
@�J�@�~@���@�qv@���@�n�@��@��@��@���@�|�@�7L@��	@��?@�tT@�$�@�{@���@�=@��@���@�Z�@� �@�hs@��B@�r�@�~@��0@��=@�qv@�.I@��@��@��U@�M�@��@��n@�s@�X@�H�@��@���@�Ov@���@��@�a@� i@��E@���@�ں@���@�PH@�($@���@��@�9�@��@��@�}V@�h�@�8�@��@��@�rG@�o@���@��+@�PH@�@��;@��=@�j@�N<@�<6@�#�@��@���@�q@�Ov@���@���@��7@�X@��@��B@��@�a|@�@��@��6@���@��@�T�@��c@�>B@�@�K@�P@�4@K�@~��@~_@}��@}|@}Vm@}@@|~(@|<�@{��@{n/@z�x@z=q@z!�@y�@yu�@x�	@xe�@w�;@w�@v�]@v�<@vv�@u��@u�@tɆ@t:�@s�V@s8@rC�@q��@q��@qN<@p��@p��@pl"@o��@o"�@nZ�@m�^@m^�@lی@lN�@k�a@j�@jW�@j�@i�@i`B@iO�@i5�@iV@h�v@h��@h`�@g��@g1�@f��@fM�@e�D@e��@e�=@eT�@e+@dѷ@dy>@c��@c��@c�@b��@bq�@a��@a�M@a#�@`�v@`��@_�r@_x@_A�@^�B@^�A@]��@]��@]@@\֡@\�D@\b@[�}@[o�@[&@[S@Z�B@Z=q@Z@Y�S@X��@X��@X6@W�;@Wx@W;d@W�@V�@V�L@VGE@V+k@V�@V	@U�=@Up�@U-w@U�@T�$@T�@S��@R�@R#:@Q��@Q�~@Qf�@QIR@QV@P�@P��@PtT@P<�@O��@O�@N��@N�F@N	@M�D@M��@M�@L��@L��@L-�@K˒@K8@J��@Jd�@I��@I��@Ik�@Ia�@IO�@I<6@I-w@I@@H�@H�u@H2�@G�m@G��@G;d@F��@Fȴ@Fff@F �@E��@E^�@Dw�@C��@C��@CX�@C4�@B�@B�L@Bc @B+k@A��@A��@Af�@A!�@@�f@@l"@?�r@?�g@?��@?8@>��@>��@>@=�S@=7L@<�@<�U@<oi@<-�@<  @;��@;]�@;�@:�@:l�@9��@9�@9�z@9��@9�M@9L�@8�P@8��@8Xy@8?�@8�@7��@6�"@6c @6�@5ϫ@5��@5\�@55�@5*0@5%F@5 \@5�@4�@4Ĝ@4e�@3ݘ@3��@3�4@2��@2�@2kQ@24@1�@1S&@0�@0�O@0|�@0/�@/��@/˒@/�:@/Mj@/'�@.��@._�@.C�@-��@-��@-[W@-F@-(�@-	l@,�@,��@,`�@,1@+�}@+��@+'�@*�!@*~�@*p;@*n�@*6�@)�>@)��@)��@)f�@)Dg@);@(�@(`�@(  @'��@'iD@'F�@'&@'�@&��@&�@&~�@&#:@%��@%�"@%k�@$��@$tT@$"h@#�@#�:@#dZ@#9�@"�@"�+@"p;@"W�@"!�@!@ ��@ y>@ Z@ M@ D�@ 6@ �@�6@�0@�f@C�@�@ i@�M@�]@ȴ@��@=q@{@��@�h@8�@%F@��@�U@�@4n@�&@��@>�@�y@~�@R�@:*@4@�D@��@@��@\�@N<@7L@�5@��@e�@?�@>B@�@��@�P@iD@F�@�@�L@�@n�@�@��@�@s�@5�@�@��@�@q@7�@�@� @��@�	@P�@�@�@��@� @@�@�@�@�H@��@}�@j@N<@8�@�@�@�@�@�[@j�@�@�s@��@��@q�@Ta@($@ �@�@��@o @Q�@@�@�@�|@�E@�z@��@��@�.@S�@�@��@�q@|�@l�@a@/�@�@�@�@
��@
p;@
=q@
+k@
4@	��@	�X@	��@	��@	`B@	8�@	;@�p@��@�@m�@bN@K^@ �@�;@ƨ@�0@��@�	@�4@v`@]�@C�@9�@�@�@�@��@�+@a|@_�@E�@
�@ԕ@��@|@\�@&�@�@��@�@�?@�j@��@��@��@��@y>@2�@�@��@\)@K�@1�@�@�@�@�@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�BB�BMBaB�BBFBB9B�B?BYB_B�B�BP}B�,B	��B	��B	��B

	B
�B
-B
3hB
dB
;JB
M�B
^�B
[�B
[WB
W�B
\�B
i�B
��B
�<B
��B
cB
{�B
{�B
��B
�fB
��B
�[B
�.B�BB"B �B
��B
�ZB	�BK�B �B
�B
�B
�4B
�B
��B
��B
��B
}�B
��B
��B
oiB
b4B
IB
+�B
�B
 �B	��B	�B	�B	�B	�B	qAB	Z�B	L�B	8�B	'8B	sB	�B�fB	[B	  B�B��B�KB�
B��B�QB�B��B�B��B�B�B��B��B	�B	�B	"NB	#B	# B	 vB	�B	"�B	$@B	($B	�B	qB	!bB	$�B	/�B	D�B	VB	KDB	UMB	`'B	g�B	m]B	~(B	��B	�EB	��B	��B	��B	�&B	��B	��B	�B	�kB	��B	�B	��B	��B	��B	�cB	�iB	��B	��B	��B	�2B	��B	�RB	�B	��B	�B	��B	�RB	��B	��B	��B	�B	�9B	��B	�|B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	�YB	�?B	�B	��B	�rB	�#B	�xB	��B	��B	�XB	�"B	�wB	�aB	��B	�	B	�RB	��B	�XB	�0B	��B	�(B	�"B	��B	��B	��B	�LB	�zB	��B	�'B	�iB	�WB	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�HB	��B	�"B	��B	�YB	�?B	�qB	��B	�B	�	B	�)B	�vB	��B	�XB	�B	�B	�B	��B	�vB	�UB	��B	�B	��B	��B	��B	�B	�~B	�B	��B	��B	��B	�]B	��B	�5B	��B	��B	�-B	�bB	��B	��B	��B	�`B	�zB	��B	�$B	��B	�DB	�B	�QB	��B	�OB	��B	�jB	��B	�UB	��B	ňB	�BB	өB	өB	��B	՛B	�sB	��B	�pB	�vB	�-B	��B	�\B	�B	��B	�LB	�B	��B	� B	��B	�iB	�B	��B	�B	�B	�}B	�B	�}B	� B	�5B	�B	��B	��B	�B	��B	�AB	�B	�B	�vB	��B	�B	��B	��B	�B	�B	�B	�nB	��B	�%B	��B	�`B	��B	��B	�LB	��B	�B	�RB	��B	��B	��B	��B	�2B	��B	��B	��B	��B	��B	��B	�8B	��B	��B	�rB	�xB	��B	�jB	�6B	��B	�B	��B	��B	�dB	��B	�jB	�VB	��B	��B	��B	��B	��B	�B	�}B	��B
 OB
�B
�B
�B
�B
{B
�B
B
�B
'B
�B
�B
�B
{B
MB
MB
gB
gB
�B
�B
�B
YB
B
?B
�B
�B
�B
B
	B
	B
	B
	�B
	�B
	�B

#B
xB
�B
�B
dB
~B
B
PB
PB
6B
6B
�B
�B
B
B
VB
�B
�B
(B
vB
�B
�B
HB
�B
B
NB
TB
:B
TB
�B
�B
�B
�B
�B
�B
�B
�B
,B
FB
B
2B
�B
B
9B
SB
mB
SB
SB
B
mB
�B
�B

B
�B
�B
B
�B
�B
EB
B
�B
�B
#B
qB
�B
)B
)B
)B
xB
dB
�B
�B
B
B
�B
�B
B
pB
;B
pB
�B
 \B
 �B
!�B
"hB
"4B
"B
"4B
!�B
!�B
"�B
#�B
$@B
$ZB
$ZB
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(>B
(�B
)DB
)�B
)�B
)�B
)�B
+�B
+�B
+�B
,�B
-)B
-wB
-�B
-�B
.B
.�B
/iB
/�B
/�B
0!B
0UB
0�B
0�B
1'B
2-B
2GB
2|B
2�B
2aB
2aB
2�B
3MB
3�B
4B
49B
49B
4�B
4�B
4�B
5tB
6�B
7B
6�B
6�B
72B
7fB
7�B
88B
8�B
8�B
8�B
8�B
8�B
9XB
9	B
9>B
9�B
9�B
:�B
:�B
:�B
:�B
;0B
;0B
;0B
;�B
;�B
<�B
<�B
=B
=qB
=�B
=�B
>wB
>�B
>�B
>�B
?HB
?.B
?HB
?HB
?HB
?cB
?cB
?�B
?�B
?�B
?�B
?�B
@ B
@B
@4B
@OB
@iB
@�B
A;B
A B
AUB
A�B
A�B
A�B
BB
B[B
B[B
B�B
CB
CaB
C{B
DB
DMB
D�B
D�B
ESB
EmB
ESB
ESB
E�B
E�B
F%B
E�B
E�B
F?B
FYB
F�B
GEB
GEB
G�B
G�B
H1B
HfB
H�B
H�B
H�B
I7B
I7B
I7B
I7B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K)B
K�B
LB
LB
LJB
LJB
L~B
L~B
L�B
L�B
L�B
MB
M�B
MjB
M�B
NVB
N<B
NVB
N�B
NVB
N<B
N�B
NpB
OBB
O�B
P.B
P�B
P�B
P�B
P�B
Q B
QB
Q B
QB
QB
QNB
Q�B
Q�B
RB
R�B
R�B
R�B
R�B
S[B
S�B
SuB
TFB
T�B
T�B
T�B
T�B
T�B
UgB
U�B
U�B
VB
V9B
V9B
VmB
V9B
W
B
W
B
W
B
WYB
XB
XyB
X�B
X�B
YB
Y�B
ZB
ZB
ZQB
ZkB
ZkB
Z�B
Z�B
[	B
[	B
[�B
[�B
[�B
\B
\B
\B
\)B
\]B
\�B
\�B
\�B
\�B
]/B
]�B
^5B
^jB
^�B
^�B
_B
_!B
_;B
_!B
_!B
_;B
_;B
_pB
_�B
`B
`B
`B
`�B
`�B
`�B
aB
abB
a|B
a�B
a�B
bB
bNB
bhB
b�B
b�B
b�B
b�B
cTB
c�B
cnB
c�B
d&B
d@B
dZB
dZB
dZB
d�B
d�B
d�B
e,B
eFB
e`B
e�B
e�B
ffB
f�B
f�B
g8B
gB
gB
g�B
gmB
gmB
g�B
h�B
h�B
iB
i_B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
jB
j�B
j�B
j�B
k6B
k�B
k�B
lB
l�B
l�B
l�B
mB
mCB
m)B
m)B
mCB
mwB
n/B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
oOB
o�B
o�B
o�B
o�B
o�B
pB
o�B
pB
p;B
p!B
p!B
pUB
p�B
p�B
p�B
qB
qvB
q�B
q�B
q�B
r-B
r�B
r�B
r�B
sB
sMB
shB
sMB
s�B
tB
tB
tB
tB
tTB
t�B
t�B
t�B
t�B
uB
u?B
utB
uZB
utB
u�B
v+B
vB
vB
v�B
v�B
v�B
v�B
w2B
wLB
wfB
w�B
w�B
w�B
xB
x8B
xRB
x�B
x�B
x�B
y$B
y$B
y>B
y�B
y�B
y�B
y�B
z*B
z^B
z^B
zxB
zxB
z^B
z�B
{JB
{�B
{�B
{�B
|B
|PB
|PB
|�B
|�B
|�B
|�B
}"B
}<B
}qB
}�B
}�B
~B
~B
~(B
~B
~(B
~BB
~]B
~]B
~BB
~�B
~�B
.B
cB
�B
�B
�B
�B
�B
� B
� B
�B
��B
��B
��B
��B
� B
�UB
�UB
�UB
��B
��B
��B
�B
�[B
�uB
��B
�B
�aB
�aB
��B
�B
�B
�3B
�MB
�3B
�MB
�gB
�gB
�MB
��B
�MB
��B
��B
�B
�SB
�B
�SB
��B
��B
�%B
�%B
�?B
�?B
�YB
�tB
�tB
��B
��B
��B
��B
��B
��B
��B
�_B
��B
�1B
��B
��B
��B
�B
�B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�BB�BMBaB�BBFBB9B�B?BYB_B�B�BP}B�,B	��B	��B	��B

	B
�B
-B
3hB
dB
;JB
M�B
^�B
[�B
[WB
W�B
\�B
i�B
��B
�<B
��B
cB
{�B
{�B
��B
�fB
��B
�[B
�.B�BB"B �B
��B
�ZB	�BK�B �B
�B
�B
�4B
�B
��B
��B
��B
}�B
��B
��B
oiB
b4B
IB
+�B
�B
 �B	��B	�B	�B	�B	�B	qAB	Z�B	L�B	8�B	'8B	sB	�B�fB	[B	  B�B��B�KB�
B��B�QB�B��B�B��B�B�B��B��B	�B	�B	"NB	#B	# B	 vB	�B	"�B	$@B	($B	�B	qB	!bB	$�B	/�B	D�B	VB	KDB	UMB	`'B	g�B	m]B	~(B	��B	�EB	��B	��B	��B	�&B	��B	��B	�B	�kB	��B	�B	��B	��B	��B	�cB	�iB	��B	��B	��B	�2B	��B	�RB	�B	��B	�B	��B	�RB	��B	��B	��B	�B	�9B	��B	�|B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	�YB	�?B	�B	��B	�rB	�#B	�xB	��B	��B	�XB	�"B	�wB	�aB	��B	�	B	�RB	��B	�XB	�0B	��B	�(B	�"B	��B	��B	��B	�LB	�zB	��B	�'B	�iB	�WB	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�HB	��B	�"B	��B	�YB	�?B	�qB	��B	�B	�	B	�)B	�vB	��B	�XB	�B	�B	�B	��B	�vB	�UB	��B	�B	��B	��B	��B	�B	�~B	�B	��B	��B	��B	�]B	��B	�5B	��B	��B	�-B	�bB	��B	��B	��B	�`B	�zB	��B	�$B	��B	�DB	�B	�QB	��B	�OB	��B	�jB	��B	�UB	��B	ňB	�BB	өB	өB	��B	՛B	�sB	��B	�pB	�vB	�-B	��B	�\B	�B	��B	�LB	�B	��B	� B	��B	�iB	�B	��B	�B	�B	�}B	�B	�}B	� B	�5B	�B	��B	��B	�B	��B	�AB	�B	�B	�vB	��B	�B	��B	��B	�B	�B	�B	�nB	��B	�%B	��B	�`B	��B	��B	�LB	��B	�B	�RB	��B	��B	��B	��B	�2B	��B	��B	��B	��B	��B	��B	�8B	��B	��B	�rB	�xB	��B	�jB	�6B	��B	�B	��B	��B	�dB	��B	�jB	�VB	��B	��B	��B	��B	��B	�B	�}B	��B
 OB
�B
�B
�B
�B
{B
�B
B
�B
'B
�B
�B
�B
{B
MB
MB
gB
gB
�B
�B
�B
YB
B
?B
�B
�B
�B
B
	B
	B
	B
	�B
	�B
	�B

#B
xB
�B
�B
dB
~B
B
PB
PB
6B
6B
�B
�B
B
B
VB
�B
�B
(B
vB
�B
�B
HB
�B
B
NB
TB
:B
TB
�B
�B
�B
�B
�B
�B
�B
�B
,B
FB
B
2B
�B
B
9B
SB
mB
SB
SB
B
mB
�B
�B

B
�B
�B
B
�B
�B
EB
B
�B
�B
#B
qB
�B
)B
)B
)B
xB
dB
�B
�B
B
B
�B
�B
B
pB
;B
pB
�B
 \B
 �B
!�B
"hB
"4B
"B
"4B
!�B
!�B
"�B
#�B
$@B
$ZB
$ZB
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(>B
(�B
)DB
)�B
)�B
)�B
)�B
+�B
+�B
+�B
,�B
-)B
-wB
-�B
-�B
.B
.�B
/iB
/�B
/�B
0!B
0UB
0�B
0�B
1'B
2-B
2GB
2|B
2�B
2aB
2aB
2�B
3MB
3�B
4B
49B
49B
4�B
4�B
4�B
5tB
6�B
7B
6�B
6�B
72B
7fB
7�B
88B
8�B
8�B
8�B
8�B
8�B
9XB
9	B
9>B
9�B
9�B
:�B
:�B
:�B
:�B
;0B
;0B
;0B
;�B
;�B
<�B
<�B
=B
=qB
=�B
=�B
>wB
>�B
>�B
>�B
?HB
?.B
?HB
?HB
?HB
?cB
?cB
?�B
?�B
?�B
?�B
?�B
@ B
@B
@4B
@OB
@iB
@�B
A;B
A B
AUB
A�B
A�B
A�B
BB
B[B
B[B
B�B
CB
CaB
C{B
DB
DMB
D�B
D�B
ESB
EmB
ESB
ESB
E�B
E�B
F%B
E�B
E�B
F?B
FYB
F�B
GEB
GEB
G�B
G�B
H1B
HfB
H�B
H�B
H�B
I7B
I7B
I7B
I7B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K)B
K�B
LB
LB
LJB
LJB
L~B
L~B
L�B
L�B
L�B
MB
M�B
MjB
M�B
NVB
N<B
NVB
N�B
NVB
N<B
N�B
NpB
OBB
O�B
P.B
P�B
P�B
P�B
P�B
Q B
QB
Q B
QB
QB
QNB
Q�B
Q�B
RB
R�B
R�B
R�B
R�B
S[B
S�B
SuB
TFB
T�B
T�B
T�B
T�B
T�B
UgB
U�B
U�B
VB
V9B
V9B
VmB
V9B
W
B
W
B
W
B
WYB
XB
XyB
X�B
X�B
YB
Y�B
ZB
ZB
ZQB
ZkB
ZkB
Z�B
Z�B
[	B
[	B
[�B
[�B
[�B
\B
\B
\B
\)B
\]B
\�B
\�B
\�B
\�B
]/B
]�B
^5B
^jB
^�B
^�B
_B
_!B
_;B
_!B
_!B
_;B
_;B
_pB
_�B
`B
`B
`B
`�B
`�B
`�B
aB
abB
a|B
a�B
a�B
bB
bNB
bhB
b�B
b�B
b�B
b�B
cTB
c�B
cnB
c�B
d&B
d@B
dZB
dZB
dZB
d�B
d�B
d�B
e,B
eFB
e`B
e�B
e�B
ffB
f�B
f�B
g8B
gB
gB
g�B
gmB
gmB
g�B
h�B
h�B
iB
i_B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
jB
j�B
j�B
j�B
k6B
k�B
k�B
lB
l�B
l�B
l�B
mB
mCB
m)B
m)B
mCB
mwB
n/B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
oOB
o�B
o�B
o�B
o�B
o�B
pB
o�B
pB
p;B
p!B
p!B
pUB
p�B
p�B
p�B
qB
qvB
q�B
q�B
q�B
r-B
r�B
r�B
r�B
sB
sMB
shB
sMB
s�B
tB
tB
tB
tB
tTB
t�B
t�B
t�B
t�B
uB
u?B
utB
uZB
utB
u�B
v+B
vB
vB
v�B
v�B
v�B
v�B
w2B
wLB
wfB
w�B
w�B
w�B
xB
x8B
xRB
x�B
x�B
x�B
y$B
y$B
y>B
y�B
y�B
y�B
y�B
z*B
z^B
z^B
zxB
zxB
z^B
z�B
{JB
{�B
{�B
{�B
|B
|PB
|PB
|�B
|�B
|�B
|�B
}"B
}<B
}qB
}�B
}�B
~B
~B
~(B
~B
~(B
~BB
~]B
~]B
~BB
~�B
~�B
.B
cB
�B
�B
�B
�B
�B
� B
� B
�B
��B
��B
��B
��B
� B
�UB
�UB
�UB
��B
��B
��B
�B
�[B
�uB
��B
�B
�aB
�aB
��B
�B
�B
�3B
�MB
�3B
�MB
�gB
�gB
�MB
��B
�MB
��B
��B
�B
�SB
�B
�SB
��B
��B
�%B
�%B
�?B
�?B
�YB
�tB
�tB
��B
��B
��B
��B
��B
��B
��B
�_B
��B
�1B
��B
��B
��B
�B
�B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105240  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192133  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192133  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192133                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042142  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042142  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151509                      G�O�G�O�G�O�                