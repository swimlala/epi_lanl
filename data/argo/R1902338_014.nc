CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2020-05-14T00:37:47Z creation;2020-05-14T00:37:49Z conversion to V3.1      
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
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݐ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �L   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �lArgo profile    3.1 1.2 19500101000000  20200514003747  20200514005615  1902338                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   NAVIS_A                         0922                            ARGO                            863 @�5FZC�1   @�6��Y �DNz�G�@DSn��P1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D�fD  D� D  D�fDfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ Dм�D�  D�@ Dр D�� D�  D�@ D�|�D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@\(@�G�@�G�A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{B�aHB�{B�{B�ǮB�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc��Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dx�D�Dr�D�Dx�D��Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D��Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dx�D��Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI��DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drx�Dr�Dsr�Ds�Dtr�Dt�Dur�Du�Dvr�Dv�Dwr�Dw�Dxr�Dx�Dyr�Dy�Dzr�Dz�D{r�D{�D|r�D|�D}r�D}�D~r�D~�Dr�D�D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�|{D��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�<{D�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD¹HD��HD�9HD�yHDùHD��HD�9HD�yHDĹHD��HD�9HD�yHDŹHD��HD�9HD�yHDƹHD��HD�9HD�yHDǹHD��HD�9HD�yHDȹHD��HD�9HD�yHDɹHD��HD�9HD�yHDʹHD��HD�9HD�yHD˹HD��HD�9HD�yHD̹HD��HD�9HD�yHD͹HD��HD�9HD�yHDιHD��HD�9HD�yHDϹHD��HD�9HD�yHDжD��HD�9HD�yHDѹHD��HD�9HD�vDҹHD��HD�9HD�yHDӹHD��HD�9HD�yHDԹHD��HD�9HD�yHDչHD��HD�9HD�yHDֹHD��HD�9HD�yHD׹HD��HD�9HD�yHDعHD��HD�9HD�yHDٹHD��HD�9HD�yHDڹHD��HD�9HD�yHD۹HD��HD�9HD�yHDܹHD��HD�9HD�yHDݹHD��HD�9HD�yHD޹HD��HD�9HD�yHD߹HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�6D�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD�D�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Au�7Au�Au�hAu��Au�PAu�PAu��Au��Au��Au��Au��Au��Au��Au��Au�wAuƨAu��Au��Au�
Au�
Au�-Au�-Au�-Au�FAu�FAu�FAuƨAu�Au�#Au�Au�Au��Au��Au�AvAv�Av�AvJAvJAv$�Av=qAvE�AvI�AvI�AvQ�Av�uAv��Av��Aw
=AwhsAw�Ax�AwhsAul�AsApr�Aol�Am�mAlr�Aj�yAi�FAh�DAfjAet�Ae\)Ae33Ad��Ac7LA`�A`-A_��A_�A^bA]�A\�uA[�AZ�+AYdZAYVAXr�AW��AUO�AUS�AU%AS�ARjAQ&�APAO\)AO"�AN��AN�AM��AL~�AK�
AJ��AJn�AJ5?AIAI+AH��AH�AH�AH(�AG�AG��AF��AF��AFVAE�mAD�yAC%AAƨAAp�AA
=A@�A@A�A?�A?��A>�A>�A=C�A=A;�;A:�A:��A:M�A9�A8��A8��A8�\A8-A7�#A7?}A6��A6(�A5K�A4��A4r�A4M�A4-A3�A3S�A3�A2�9A1�A0��A/l�A/&�A.��A.bNA-A-t�A-`BA-C�A-VA,��A,��A,v�A,{A,A+�A+��A+&�A+VA*�yA*ȴA*�A*(�A)�FA)O�A(�9A(��A(�+A(r�A(ZA(�A'p�A&��A&�uA&ZA%��A%l�A%/A$��A$��A$JA"��A"��A"Q�A"�A!�#A!;dA �!A�#A�AS�A�A�A�A�A��A��A��A�uA��A��A��A��A\)A�A�HA�!A�DA5?A�A��A�\Al�A�`A�jA�AI�A�A��A��A+A��A�A^5A��A7LA�A1'A��At�AO�A7LAVA��AffA5?A�
A��A�7At�A"�A�DA �A��A��A?}A�A�A��AO�AĜA5?A|�A
��A
r�A
E�A	��A	hsA	A��AZA5?A��A�hA;dA��AjA(�A��A�A33A��AQ�A�AXA�A�A ��A ��A (�@���@�$�@�r�@��@��@�ȴ@�/@�  @�;d@�@�@�h@���@� �@�-@�C�@��@�R@��@�E�@���@�l�@��@�@�&�@��/@�u@�bN@�@�7@�9X@���@�t�@��y@ݺ^@��@�K�@�=q@�hs@���@׶F@�V@ԋD@�~�@ѡ�@�ƨ@���@ͺ^@��@� �@���@�G�@�1'@ǝ�@��@�5?@�/@ēu@��@��@��;@öF@�K�@�$�@�p�@���@�Q�@�I�@�9X@��@��
@��@���@���@�%@��@��@���@�;d@�E�@�@���@�X@��u@���@�|�@�33@���@�{@��@��F@��@�ȴ@�~�@�@��9@�p�@�n�@�Z@��@���@�n�@���@���@���@���@��#@��#@���@���@�=q@��@���@��9@�@���@�V@�=q@���@�  @�r�@��/@��`@���@�z�@��H@��u@��\@�$�@���@�Ĝ@���@�~�@�
=@���@���@��!@��!@��\@��T@��@�5?@�@��@�-@�M�@��@���@�`B@�O�@��/@�9X@�  @���@���@��@��^@�@���@��@��\@�E�@���@���@���@���@��+@�t�@���@�{@��j@�V@�G�@�?}@���@�r�@�j@�j@�r�@�r�@�9X@��@�~�@��^@�p�@���@��u@��@�33@�S�@� �@�J@��
@�r�@���@�Ĝ@�Ĝ@��h@�M�@��\@�-@��@�x�@��u@�1'@�1@���@���@��u@�I�@��D@��@��@� �@~�@+@�bN@� �@��@�w@�w@��@�;@K�@~�R@~$�@}O�@}V@|�@|�@|��@|�j@|�@|��@|z�@|z�@|9X@{ƨ@{��@{�F@{�m@{�m@{�m@{�
@{ƨ@{��@{�@{�@{t�@{o@zM�@yX@x�9@w�@v�+@v{@t1@sdZ@st�@s��@s��@s�@sS�@s@r^5@q�^@q��@q%@o��@n5?@n@m�h@m��@nff@n{@m@l��@kƨ@kt�@k33@kC�@kC�@k33@j~�@j��@j��@k"�@kS�@k�@kC�@jn�@i��@ihs@h�9@g�;@e`B@ct�@cdZ@b��@a�@a�@`Ĝ@` �@_�@_\)@_;d@]`B@Z-@ZJ@Y7L@V��@Up�@T��@TZ@S�@S@R��@RM�@RJ@Q�^@Q�7@Q�7@Q�7@Q�7@Qx�@QG�@P�9@PA�@Pb@O�w@O��@O|�@O
=@N�R@N��@N�+@NE�@N5?@NE�@NV@NE�@N5?@N{@M�@N$�@Mp�@L�@K�@Jn�@J=q@I�@IX@I&�@H��@H��@HQ�@G�@G�w@G��@G\)@G+@G�@G�@G
=@F��@F��@F�y@F�R@F�+@F$�@F{@E�@E�T@EO�@D��@D�D@Dz�@D9X@C��@CC�@B�@B�!@B~�@BM�@B=q@B=q@B-@BJ@BJ@BJ@A��@A��@A��@A��@A��@A��@A�@A�#@A�^@A��@A��@A��@Ax�@Ax�@Ahs@AX@AG�@A&�@A�@A%@@�`@@�@@  @?l�@?|�@?�P@?��@?��@?�@@  @?��@?�@?��@>��@>�+@>V@=�@=��@=@=�-@=�@<�@<z�@<I�@<9X@<(�@<�@<�@;�m@;ƨ@;�F@;��@;t�@;"�@:��@:�@9�@9��@9x�@9hs@9hs@9x�@9�7@9�^@:�@:-@:=q@:M�@:n�@:M�@:J@9�^@9X@97L@9&�@9&�@9&�@9�@9�@9&�@9&�@9&�@97L@9�@8��@8 �@7�@7�@7�@7�@7�;@7��@7�w@7�P@7|�@7K�@7+@7+@7�@7
=@7
=@7
=@6��@6�y@6�@6�y@6�@6ȴ@6ȴ@6��@6V@5�T@5@5�-@5�h@5`B@5?}@5?}@5?}@4��@4�@4�@4��@4�@4��@4��@4z�@4Z@4I�@49X@49X@49X@49X@49X@49X@4�@3�
@3��@3t�@333@3"�@3o@2�@2��@2~�@1��@1��@1��@1��@1��@1��@1hs@1X@1&�@1%@0�`@0�`@0��@0Ĝ@0Ĝ@0�9@0�9@0��@0�u@0�@0r�@0r�@0r�@0bN@0bN@0Q�@0Q�@01'@0 �@0b@0  @/�;@/��@/��@/��@/�@/��@/�P@/�P@/|�@/|�@/l�@/\)@/\)@/\)@/+@/
=@.��@.�y@.�y@.�@.ȴ@.ȴ@.ȴ@.ȴ@.ȴ@.ȴ@.ȴ@.�@.�@.�@.�y@.��@.��@/
=@/
=@/
=@/�@/+@/+@/+@/+@/+@/;d@/;d@/K�@/K�@/\)@/\)@/\)@/\)@/|�@/�P@/�@/�w@/�;@/�;@/�;@/�@/�@/�@/�@/�;@/�;@/�w@/�w@/�P@/l�@/l�@/\)@/;d@/+@/;d@/K�@/K�@/K�@/K�@/K�@/\)@/\)@/K�@/\)@/\)@/K�@/\)@/\)@/\)@/\)@/\)@/K�@/�@/
=@/
=@.��@.�@.�y@.��@.ȴ@.v�@.ff@.V@.V@.V@.V@.E�@.5?@.E�@.V@.E�@.{@-�@-�@-@-�@-`B@-O�@-O�@-O�@-O�@-O�@-?}@-?}@-?}@-/@-/@-/@-/@-/@-/@-�@-�@-V@-V@-V@-V@,��@,��@,�j@,�D@,z�@,z�@,j@,I�@,1@+��@+��@+�m@+�
@+�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Au�7Au�Au�hAu��Au�PAu�PAu��Au��Au��Au��Au��Au��Au��Au��Au�wAuƨAu��Au��Au�
Au�
Au�-Au�-Au�-Au�FAu�FAu�FAuƨAu�Au�#Au�Au�Au��Au��Au�AvAv�Av�AvJAvJAv$�Av=qAvE�AvI�AvI�AvQ�Av�uAv��Av��Aw
=AwhsAw�Ax�AwhsAul�AsApr�Aol�Am�mAlr�Aj�yAi�FAh�DAfjAet�Ae\)Ae33Ad��Ac7LA`�A`-A_��A_�A^bA]�A\�uA[�AZ�+AYdZAYVAXr�AW��AUO�AUS�AU%AS�ARjAQ&�APAO\)AO"�AN��AN�AM��AL~�AK�
AJ��AJn�AJ5?AIAI+AH��AH�AH�AH(�AG�AG��AF��AF��AFVAE�mAD�yAC%AAƨAAp�AA
=A@�A@A�A?�A?��A>�A>�A=C�A=A;�;A:�A:��A:M�A9�A8��A8��A8�\A8-A7�#A7?}A6��A6(�A5K�A4��A4r�A4M�A4-A3�A3S�A3�A2�9A1�A0��A/l�A/&�A.��A.bNA-A-t�A-`BA-C�A-VA,��A,��A,v�A,{A,A+�A+��A+&�A+VA*�yA*ȴA*�A*(�A)�FA)O�A(�9A(��A(�+A(r�A(ZA(�A'p�A&��A&�uA&ZA%��A%l�A%/A$��A$��A$JA"��A"��A"Q�A"�A!�#A!;dA �!A�#A�AS�A�A�A�A�A��A��A��A�uA��A��A��A��A\)A�A�HA�!A�DA5?A�A��A�\Al�A�`A�jA�AI�A�A��A��A+A��A�A^5A��A7LA�A1'A��At�AO�A7LAVA��AffA5?A�
A��A�7At�A"�A�DA �A��A��A?}A�A�A��AO�AĜA5?A|�A
��A
r�A
E�A	��A	hsA	A��AZA5?A��A�hA;dA��AjA(�A��A�A33A��AQ�A�AXA�A�A ��A ��A (�@���@�$�@�r�@��@��@�ȴ@�/@�  @�;d@�@�@�h@���@� �@�-@�C�@��@�R@��@�E�@���@�l�@��@�@�&�@��/@�u@�bN@�@�7@�9X@���@�t�@��y@ݺ^@��@�K�@�=q@�hs@���@׶F@�V@ԋD@�~�@ѡ�@�ƨ@���@ͺ^@��@� �@���@�G�@�1'@ǝ�@��@�5?@�/@ēu@��@��@��;@öF@�K�@�$�@�p�@���@�Q�@�I�@�9X@��@��
@��@���@���@�%@��@��@���@�;d@�E�@�@���@�X@��u@���@�|�@�33@���@�{@��@��F@��@�ȴ@�~�@�@��9@�p�@�n�@�Z@��@���@�n�@���@���@���@���@��#@��#@���@���@�=q@��@���@��9@�@���@�V@�=q@���@�  @�r�@��/@��`@���@�z�@��H@��u@��\@�$�@���@�Ĝ@���@�~�@�
=@���@���@��!@��!@��\@��T@��@�5?@�@��@�-@�M�@��@���@�`B@�O�@��/@�9X@�  @���@���@��@��^@�@���@��@��\@�E�@���@���@���@���@��+@�t�@���@�{@��j@�V@�G�@�?}@���@�r�@�j@�j@�r�@�r�@�9X@��@�~�@��^@�p�@���@��u@��@�33@�S�@� �@�J@��
@�r�@���@�Ĝ@�Ĝ@��h@�M�@��\@�-@��@�x�@��u@�1'@�1@���@���@��u@�I�@��D@��@��@� �@~�@+@�bN@� �@��@�w@�w@��@�;@K�@~�R@~$�@}O�@}V@|�@|�@|��@|�j@|�@|��@|z�@|z�@|9X@{ƨ@{��@{�F@{�m@{�m@{�m@{�
@{ƨ@{��@{�@{�@{t�@{o@zM�@yX@x�9@w�@v�+@v{@t1@sdZ@st�@s��@s��@s�@sS�@s@r^5@q�^@q��@q%@o��@n5?@n@m�h@m��@nff@n{@m@l��@kƨ@kt�@k33@kC�@kC�@k33@j~�@j��@j��@k"�@kS�@k�@kC�@jn�@i��@ihs@h�9@g�;@e`B@ct�@cdZ@b��@a�@a�@`Ĝ@` �@_�@_\)@_;d@]`B@Z-@ZJ@Y7L@V��@Up�@T��@TZ@S�@S@R��@RM�@RJ@Q�^@Q�7@Q�7@Q�7@Q�7@Qx�@QG�@P�9@PA�@Pb@O�w@O��@O|�@O
=@N�R@N��@N�+@NE�@N5?@NE�@NV@NE�@N5?@N{@M�@N$�@Mp�@L�@K�@Jn�@J=q@I�@IX@I&�@H��@H��@HQ�@G�@G�w@G��@G\)@G+@G�@G�@G
=@F��@F��@F�y@F�R@F�+@F$�@F{@E�@E�T@EO�@D��@D�D@Dz�@D9X@C��@CC�@B�@B�!@B~�@BM�@B=q@B=q@B-@BJ@BJ@BJ@A��@A��@A��@A��@A��@A��@A�@A�#@A�^@A��@A��@A��@Ax�@Ax�@Ahs@AX@AG�@A&�@A�@A%@@�`@@�@@  @?l�@?|�@?�P@?��@?��@?�@@  @?��@?�@?��@>��@>�+@>V@=�@=��@=@=�-@=�@<�@<z�@<I�@<9X@<(�@<�@<�@;�m@;ƨ@;�F@;��@;t�@;"�@:��@:�@9�@9��@9x�@9hs@9hs@9x�@9�7@9�^@:�@:-@:=q@:M�@:n�@:M�@:J@9�^@9X@97L@9&�@9&�@9&�@9�@9�@9&�@9&�@9&�@97L@9�@8��@8 �@7�@7�@7�@7�@7�;@7��@7�w@7�P@7|�@7K�@7+@7+@7�@7
=@7
=@7
=@6��@6�y@6�@6�y@6�@6ȴ@6ȴ@6��@6V@5�T@5@5�-@5�h@5`B@5?}@5?}@5?}@4��@4�@4�@4��@4�@4��@4��@4z�@4Z@4I�@49X@49X@49X@49X@49X@49X@4�@3�
@3��@3t�@333@3"�@3o@2�@2��@2~�@1��@1��@1��@1��@1��@1��@1hs@1X@1&�@1%@0�`@0�`@0��@0Ĝ@0Ĝ@0�9@0�9@0��@0�u@0�@0r�@0r�@0r�@0bN@0bN@0Q�@0Q�@01'@0 �@0b@0  @/�;@/��@/��@/��@/�@/��@/�P@/�P@/|�@/|�@/l�@/\)@/\)@/\)@/+@/
=@.��@.�y@.�y@.�@.ȴ@.ȴ@.ȴ@.ȴ@.ȴ@.ȴ@.ȴ@.�@.�@.�@.�y@.��@.��@/
=@/
=@/
=@/�@/+@/+@/+@/+@/+@/;d@/;d@/K�@/K�@/\)@/\)@/\)@/\)@/|�@/�P@/�@/�w@/�;@/�;@/�;@/�@/�@/�@/�@/�;@/�;@/�w@/�w@/�P@/l�@/l�@/\)@/;d@/+@/;d@/K�@/K�@/K�@/K�@/K�@/\)@/\)@/K�@/\)@/\)@/K�@/\)@/\)@/\)@/\)@/\)@/K�@/�@/
=@/
=@.��@.�@.�y@.��@.ȴ@.v�@.ff@.V@.V@.V@.V@.E�@.5?@.E�@.V@.E�@.{@-�@-�@-@-�@-`B@-O�@-O�@-O�@-O�@-O�@-?}@-?}@-?}@-/@-/@-/@-/@-/@-/@-�@-�@-V@-V@-V@-V@,��@,��@,�j@,�D@,z�@,z�@,j@,I�@,1@+��@+��@+�m@+�
@+�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�hB�hB�bB�bB�hB�hB�bB�bB�bB�hB�bB�bB�bB�bB�oB�oB�uB�uB�uB�uB�hB�bB�bB�bB�hB�hB�hB�{B�uB�{B�{B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�jBƨB�/B�B�B�B�B�B�B�B�mB�fB�sB�ZB�5B�/B�)B�B�
B��BĜB��B�jB�FB�B�B��B��B�uB�bB�PB�PBz�B|�B�Bt�Bm�BffB\)BT�BR�BO�BK�BG�B=qB8RB1'B+B(�B$�B$�B'�B'�B%�B �B�B#�B �B"�B!�B�BhB  B�B�B�B�yB�`B�TB�BB�B�B��BǮB�jB�!B�B��B��B��B��B��B��B�uB�PB�7B�Bz�Bu�Bq�Bp�Bn�Bl�BhsBdZBdZBW
BL�B?}B9XB7LB49B/B-B,B+B(�B(�B(�B(�B&�B'�B&�B$�B!�B �B�B�B�B�B�B{BhBbBbB\B\BbB\BJB1BB  B
��BBB
��B
��B
�B
�B
�B
�B
�B
�sB
�HB
�#B
�B
�B
�B
�B
�B
�B
�B
�)B
�;B
�BB
�fB
�B
�B
�B
�mB
�fB
�`B
�ZB
�TB
�TB
�HB
�NB
�;B
�5B
�B
�B
�
B
�
B
��B
��B
��B
��B
��B
��B
��B
ƨB
ÖB
�qB
�XB
�FB
�9B
�-B
�'B
�'B
�B
�B
�B
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
�oB
�PB
�=B
�B
~�B
z�B
w�B
u�B
q�B
n�B
k�B
jB
hsB
gmB
dZB
bNB
aHB
_;B
\)B
[#B
YB
W
B
S�B
P�B
P�B
K�B
E�B
A�B
>wB
=qB
;dB
9XB
7LB
2-B
/B
.B
-B
(�B
$�B
!�B
�B
�B
{B
hB
\B

=B
B
  B
  B
  B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�yB	�mB	�fB	�fB	�`B	�NB	�BB	�#B	�B	�B	�B	�B	��B	��B	ĜB	��B	�qB	�^B	�LB	�9B	�-B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�uB	�oB	�hB	�hB	�hB	�hB	�bB	�\B	�DB	�1B	�1B	�JB	�DB	�=B	�JB	�PB	�VB	�VB	�PB	�JB	�=B	�=B	�7B	�+B	�+B	�B	� B	~�B	}�B	{�B	x�B	m�B	aHB	W
B	R�B	O�B	N�B	K�B	K�B	L�B	K�B	L�B	M�B	N�B	T�B	S�B	P�B	H�B	A�B	9XB	49B	5?B	;dB	I�B	M�B	Q�B	T�B	VB	VB	T�B	R�B	N�B	E�B	C�B	@�B	C�B	I�B	O�B	ZB	ZB	[#B	\)B	]/B	^5B	`BB	`BB	YB	YB	\)B	_;B	bNB	e`B	gmB	gmB	gmB	gmB	gmB	iyB	hsB	e`B	gmB	hsB	hsB	k�B	l�B	n�B	o�B	t�B	s�B	s�B	r�B	l�B	aHB	^5B	_;B	\)B	_;B	dZB	e`B	e`B	e`B	e`B	e`B	e`B	e`B	e`B	dZB	cTB	cTB	cTB	e`B	e`B	cTB	cTB	dZB	jB	u�B	�B	�1B	�DB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�hB	�\B	�VB	�\B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�3B	�9B	�?B	�?B	�?B	�FB	�?B	�9B	�3B	�-B	�'B	�!B	�B	�?B	�FB	�XB	�dB	�dB	�jB	�qB	�qB	�wB	�wB	�}B	�}B	�qB	��B	��B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�NB	�TB	�NB	�HB	�BB	�5B	�5B	�;B	�5B	�/B	�/B	�)B	�)B	�#B	�#B	�B	�B	�B	�#B	�/B	�#B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B
  B
  B
B
B
B
B
%B
+B
1B
1B

=B
DB
JB
JB
JB
PB
PB
PB
VB
\B
bB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
#�B
$�B
$�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
+B
+B
,B
,B
-B
-B
-B
.B
0!B
2-B
2-B
33B
49B
6FB
8RB
;dB
;dB
<jB
=qB
?}B
@�B
A�B
B�B
B�B
C�B
F�B
H�B
I�B
K�B
L�B
L�B
L�B
M�B
P�B
T�B
XB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
]/B
]/B
^5B
aHB
cTB
dZB
ffB
hsB
iyB
k�B
l�B
m�B
o�B
q�B
r�B
s�B
s�B
u�B
x�B
y�B
{�B
}�B
~�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�DB
�\B
�uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�!B
�'B
�'B
�-B
�-B
�3B
�3B
�3B
�3B
�9B
�?B
�?B
�FB
�FB
�FB
�FB
�LB
�LB
�XB
�^B
�dB
�jB
�jB
�wB
�wB
�wB
�wB
�}B
��B
B
ÖB
ÖB
ÖB
ĜB
ĜB
ŢB
ƨB
ǮB
ǮB
ȴB
ɺB
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
�
B
�B
�B
�B
�B
�#B
�)B
�)B
�)B
�/B
�/B
�5B
�5B
�5B
�;B
�;B
�;B
�BB
�BB
�BB
�BB
�HB
�NB
�NB
�TB
�TB
�ZB
�fB
�sB
�sB
�yB
�yB
�yB
�yB
�yB
�yB
�B
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
��B  BBBBBBB%B+B1B1B1B	7B	7B	7B	7B
=B
=B
=B
=B
=B
=B
=B
=B
=BDBDBDBDBDBDBJBPBVBVB\B\B\BhBoBoBuBuBu1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�hB�hB�bB�bB�hB�hB�bB�bB�bB�hB�bB�bB�bB�bB�oB�oB�uB�uB�uB�uB�hB�bB�bB�bB�hB�hB�hB�{B�uB�{B�{B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�jBƨB�/B�B�B�B�B�B�B�B�mB�fB�sB�ZB�5B�/B�)B�B�
B��BĜB��B�jB�FB�B�B��B��B�uB�bB�PB�PBz�B|�B�Bt�Bm�BffB\)BT�BR�BO�BK�BG�B=qB8RB1'B+B(�B$�B$�B'�B'�B%�B �B�B#�B �B"�B!�B�BhB  B�B�B�B�yB�`B�TB�BB�B�B��BǮB�jB�!B�B��B��B��B��B��B��B�uB�PB�7B�Bz�Bu�Bq�Bp�Bn�Bl�BhsBdZBdZBW
BL�B?}B9XB7LB49B/B-B,B+B(�B(�B(�B(�B&�B'�B&�B$�B!�B �B�B�B�B�B�B{BhBbBbB\B\BbB\BJB1BB  B
��BBB
��B
��B
�B
�B
�B
�B
�B
�sB
�HB
�#B
�B
�B
�B
�B
�B
�B
�B
�)B
�;B
�BB
�fB
�B
�B
�B
�mB
�fB
�`B
�ZB
�TB
�TB
�HB
�NB
�;B
�5B
�B
�B
�
B
�
B
��B
��B
��B
��B
��B
��B
��B
ƨB
ÖB
�qB
�XB
�FB
�9B
�-B
�'B
�'B
�B
�B
�B
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
�oB
�PB
�=B
�B
~�B
z�B
w�B
u�B
q�B
n�B
k�B
jB
hsB
gmB
dZB
bNB
aHB
_;B
\)B
[#B
YB
W
B
S�B
P�B
P�B
K�B
E�B
A�B
>wB
=qB
;dB
9XB
7LB
2-B
/B
.B
-B
(�B
$�B
!�B
�B
�B
{B
hB
\B

=B
B
  B
  B
  B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�yB	�mB	�fB	�fB	�`B	�NB	�BB	�#B	�B	�B	�B	�B	��B	��B	ĜB	��B	�qB	�^B	�LB	�9B	�-B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�uB	�oB	�hB	�hB	�hB	�hB	�bB	�\B	�DB	�1B	�1B	�JB	�DB	�=B	�JB	�PB	�VB	�VB	�PB	�JB	�=B	�=B	�7B	�+B	�+B	�B	� B	~�B	}�B	{�B	x�B	m�B	aHB	W
B	R�B	O�B	N�B	K�B	K�B	L�B	K�B	L�B	M�B	N�B	T�B	S�B	P�B	H�B	A�B	9XB	49B	5?B	;dB	I�B	M�B	Q�B	T�B	VB	VB	T�B	R�B	N�B	E�B	C�B	@�B	C�B	I�B	O�B	ZB	ZB	[#B	\)B	]/B	^5B	`BB	`BB	YB	YB	\)B	_;B	bNB	e`B	gmB	gmB	gmB	gmB	gmB	iyB	hsB	e`B	gmB	hsB	hsB	k�B	l�B	n�B	o�B	t�B	s�B	s�B	r�B	l�B	aHB	^5B	_;B	\)B	_;B	dZB	e`B	e`B	e`B	e`B	e`B	e`B	e`B	e`B	dZB	cTB	cTB	cTB	e`B	e`B	cTB	cTB	dZB	jB	u�B	�B	�1B	�DB	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�hB	�\B	�VB	�\B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�3B	�9B	�?B	�?B	�?B	�FB	�?B	�9B	�3B	�-B	�'B	�!B	�B	�?B	�FB	�XB	�dB	�dB	�jB	�qB	�qB	�wB	�wB	�}B	�}B	�qB	��B	��B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�NB	�TB	�NB	�HB	�BB	�5B	�5B	�;B	�5B	�/B	�/B	�)B	�)B	�#B	�#B	�B	�B	�B	�#B	�/B	�#B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B
  B
  B
B
B
B
B
%B
+B
1B
1B

=B
DB
JB
JB
JB
PB
PB
PB
VB
\B
bB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
#�B
$�B
$�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
+B
+B
,B
,B
-B
-B
-B
.B
0!B
2-B
2-B
33B
49B
6FB
8RB
;dB
;dB
<jB
=qB
?}B
@�B
A�B
B�B
B�B
C�B
F�B
H�B
I�B
K�B
L�B
L�B
L�B
M�B
P�B
T�B
XB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
]/B
]/B
^5B
aHB
cTB
dZB
ffB
hsB
iyB
k�B
l�B
m�B
o�B
q�B
r�B
s�B
s�B
u�B
x�B
y�B
{�B
}�B
~�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�DB
�\B
�uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�!B
�!B
�!B
�'B
�'B
�-B
�-B
�3B
�3B
�3B
�3B
�9B
�?B
�?B
�FB
�FB
�FB
�FB
�LB
�LB
�XB
�^B
�dB
�jB
�jB
�wB
�wB
�wB
�wB
�}B
��B
B
ÖB
ÖB
ÖB
ĜB
ĜB
ŢB
ƨB
ǮB
ǮB
ȴB
ɺB
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
�
B
�B
�B
�B
�B
�#B
�)B
�)B
�)B
�/B
�/B
�5B
�5B
�5B
�;B
�;B
�;B
�BB
�BB
�BB
�BB
�HB
�NB
�NB
�TB
�TB
�ZB
�fB
�sB
�sB
�yB
�yB
�yB
�yB
�yB
�yB
�B
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
��B  BBBBBBB%B+B1B1B1B	7B	7B	7B	7B
=B
=B
=B
=B
=B
=B
=B
=B
=BDBDBDBDBDBDBJBPBVBVB\B\B\BhBoBoBuBuBu1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20200514093745  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200514003747  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200514003747  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200514003748  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200514003748  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200514003748  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200514003748  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200514003748  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200514003748  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200514003749                      G�O�G�O�G�O�                JA  ARUP                                                                        20200514005615                      G�O�G�O�G�O�                