CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-04T11:01:08Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20230404110108  20230404110108  4903319 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               mA   AO  8280                            2B  A   NAVIS_A                         1159                            170425                          863 @�!��;1   @�!�[p@9�7KƧ��d�b��}1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         mA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Dfy�Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@\(@�G�@�G�A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW��CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfl)Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�Dur�Du�Dvr�Dv�Dwr�Dw�Dxr�Dx�Dyr�Dy�Dzr�Dz�D{r�D{�D|r�D|�D}r�D}�D~r�D~�Dr�D�D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��{D��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD¹HD��HD�9HD�yHDùHD��HD�9HD�yHDĹHD��HD�9HD�yHDŹHD��HD�9HD�yHDƹHD��HD�9HD�yHDǹHD��HD�9HD�yHDȹHD��HD�9HD�yHDɹHD��HD�9HD�yHDʹHD��HD�9HD�yHD˹HD��HD�9HD�yHD̹HD��HD�9HD�yHD͹HD��HD�9HD�yHDιHD��HD�9HD�yHDϹHD��HD�9HD�yHDйHD��HD�9HD�yHDѹHD��HD�9HD�yHDҹHD��HD�9HD�yHDӹHD��HD�9HD�yHDԹHD��HD�9HD�yHDչHD��HD�9HD�yHDֹHD��HD�9HD�yHD׹HD��HD�9HD�yHDعHD��HD�9HD�yHDٹHD��HD�9HD�yHDڹHD��HD�9HD�yHD۹HD��HD�9HD�yHDܹHD��HD�9HD�yHDݹHD��HD�9HD�yHD޹HD��HD�9HD�yHD߹HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��D�6D�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��{D�2�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A�O�A���A���A�z�A�XA� �A��mA���A�ĜA��^A��RA��RA���A�v�A�C�A�ƨA��jA���A��DA�VA�-A���A��A��jA���A�|�A���A�z�A�G�A�VA���A�G�A�v�A�VA�ffA��A���A�jA�A�A�$�A���A��
A�ZA��TA���A�$�A��-A�l�A�JA���A�  A��`A�S�A��!A���A��+A��A�~�A�r�A�S�A�1A�?}A���A�/A�~�A�~�A�?}A��A��+A��yA�hsA�A�%A�ĜA��\A�M�A�A�v�A�ȴA��uA��hA�VA�A��jA�7LA�v�A��/A��uA�&�A�oA�A�A�ĜA��`A��PA�G�A�A��A��-A���A���A�?}A�t�A��A�n�A��jA�bA}x�A|�yA{Az�Az-Az  Axv�AwdZAv~�At��Ar��Ar  AqK�Ao�An�!An{Am?}Al�Ah��Ag/Af�Ae�Ad��Ad�Ac�Aa��A`�A\v�AY"�AWhsAW/AW�AV�uAU�FAS"�AQ��AP��AO�ANQ�AL�+ALI�AKt�AI��AH�AG7LAFbAE��AE33AD�RADVAD  ACƨAC��AC?}A?�;A>�DA>I�A=�A="�A;G�A:z�A:1A9�;A9��A8��A8A7�A5�;A5K�A4bNA3�TA3oA1��A0(�A/�FA/O�A.�A-t�A-A,�+A,1A*VA)��A(jA'p�A&��A%p�A$JA"�yA"5?A�wA�9A`BA-A�^A/A�DA�7A�A�TA��Al�A$�AC�A�\A1A��A33A��A{A7LA��A�uAZAA�AbAG�A"�A�HA5?A
��A
$�A	A	/A9XA�`A�A�wAhsA$�A�DA-A�#A��A
=@��7@�V@�V@���@�A�@��@���@��@�-@���@�ȴ@�r�@�\)@�ȴ@�+@�v�@�^5@�V@�$�@��@���@�O�@�j@��@��#@�b@�V@�p�@�O�@�V@�1@���@��@�r�@۾w@ڏ\@��@�p�@ؼj@� �@׾w@֧�@�{@���@ӝ�@�S�@�o@�
=@ҸR@�=q@�7L@�;d@�%@�|�@ʰ!@��@�%@Ǯ@��@ư!@�^5@�$�@�@�1@å�@Ý�@�ȴ@�O�@�ƨ@��F@���@��@�+@��#@��@�G�@���@�z�@�Q�@��m@�o@�`B@�z�@�(�@��w@�"�@�n�@��@���@��m@�t�@�M�@�p�@��7@�7L@�Ĝ@�(�@�S�@�E�@�%@�bN@��;@�ƨ@���@�ȴ@�M�@�&�@��@��P@��+@���@�/@��@�I�@��@�ƨ@�dZ@�33@��@�V@��@��u@��@��@�;d@�33@��!@�M�@���@��h@��-@�X@�V@���@�r�@�Q�@�b@���@�t�@��@�~�@���@�&�@���@�Q�@� �@��@�
=@���@�O�@�&�@���@���@�A�@�1@���@��@��@��!@�~�@�ff@�5?@��@��^@��h@�X@��@��@���@�%@��^@��@��@�p�@�Ĝ@��
@���@�|�@���@���@��`@��@�  @�t�@�
=@��@��y@�v�@�{@���@���@�Ĝ@��D@��@��w@�|�@�K�@��y@��!@�v�@�ff@�^5@�5?@��T@���@��@���@�Ĝ@��/@���@���@�Ĝ@��@�9X@��P@�dZ@�o@�$�@��^@�p�@�?}@��@�V@���@��j@;d@
=@~ȴ@~��@~ff@~5?@~{@~@}@}��@}�h@}/@|�@|z�@{��@{�F@{t�@{S�@{S�@{S�@{@z^5@y��@yhs@x�9@x�u@x�@xA�@w��@v�R@v5?@u`B@t��@t�@t��@sƨ@r~�@q�#@q��@q�@pbN@pb@o��@o+@n�@nȴ@n�R@nv�@m�T@mO�@lz�@lI�@l�@k��@kC�@j��@j��@j�!@jn�@j�@i�@i�7@i7L@i�@i�@i&�@i%@h�u@h1'@g�@g�P@g;d@gK�@g+@f�@fȴ@fff@e@eO�@e�@eV@d��@dZ@c��@cƨ@c�F@ct�@c"�@c@b�@b��@a��@ahs@`Ĝ@`b@_�;@_��@_\)@_+@^��@^�@^��@^E�@^{@]��@]��@]�@\�@\1@[dZ@[@Z��@Z�\@ZJ@Y�#@Y�#@Y��@Y��@Y��@Y��@Y�#@Y�#@Y�#@Y�#@X�9@W�@W�@V��@VE�@U�-@T�j@T1@S�@R��@R-@Q��@Qx�@QX@QG�@Q&�@P��@P�`@P��@PĜ@P��@P1'@O|�@O;d@O+@N�y@Nȴ@N�R@N��@N��@N�+@Nff@NV@M�@MO�@L�j@L9X@L1@K�m@K�
@KdZ@J�@J~�@I�@Ihs@I7L@H�`@H�u@HbN@G�;@G|�@Gl�@G;d@F��@F�R@F�+@Fv�@Fff@FV@FE�@FE�@F{@E�T@E�-@D�@C�
@C�F@CS�@Co@B�@B��@B-@A�#@AG�@A&�@A%@@��@@�`@@�`@@�`@@Ĝ@?�;@?��@?;d@?�@>�@>ȴ@>�R@>V@>V@>5?@=�-@=/@<9X@;�
@;dZ@:��@:-@:J@9��@9��@9��@9��@9�@8A�@8 �@8 �@8 �@8b@8b@8b@8b@8  @7�;@7�w@7�@7��@7+@6�+@6@5�@5@5?}@4�@4z�@4I�@3�m@3��@3dZ@3"�@3@2~�@2^5@2=q@1��@0��@0��@0Q�@0b@/�;@/�@/��@/\)@.�R@.V@.5?@-��@-@-��@-�@-?}@-�@,�/@,z�@,Z@+�m@+ƨ@+�F@+��@+�@+dZ@+dZ@+C�@*�@*��@*�\@*~�@*~�@*~�@*~�@*~�@*~�@*~�@*~�@*n�@*^5@*^5@*=q@*=q@)�7@)&�@)�@(��@(��@(bN@(1'@'��@'\)@&�@&ȴ@&v�@&$�@%�@%O�@%V@$�/@$��@$j@$9X@$�@$1@#��@#�m@#�
@#�F@#��@#�@#o@"��@"��@"~�@"n�@"^5@"M�@"�@!�@!��@!&�@ ��@ Ĝ@ �9@ �u@ r�@ bN@ Q�@ A�@  �@   @�@�;@�w@�P@\)@ȴ@E�@�@�h@?}@V@�/@��@Z@9X@(�@�@1@1@��@�
@�F@t�@dZ@S�@33@"�@@�@��@�!@~�@^5@-@J@��@�7@x�@G�@�@��@ �@�;@�w@��@�P@+@�R@V@�@�-@��@�h@p�@/@�j@z�@�F@@�H@��@��@��@�\@~�@n�@M�@=q@=q@-@J@��@��@��@G�@&�@&�@��@��@�@r�@Q�@�;@|�@\)@;d@+@�@
=@��@�y@ȴ@�R@�+@V@{@�-@O�@?}@�@�/@��@�j@�D@1@��@t�@C�@33@33@33@"�@o@
�\@
^5@
M�@
M�@
M�@
=q@	�^@	��@	x�@	X@	X@	G�@	G�@	7L@	7L@	%@��@�u@bN@1'@b@  @�;@�@;d@�R@��@v�@V@5?@5?@@@@��@�h@�@�@`B@O�@?}@��@��@�@��@Z@1@��@�
@ƨ@��@33@o@@@@�@��@��@��@�!@~�@M�@�@��@hs@X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A�O�A���A���A�z�A�XA� �A��mA���A�ĜA��^A��RA��RA���A�v�A�C�A�ƨA��jA���A��DA�VA�-A���A��A��jA���A�|�A���A�z�A�G�A�VA���A�G�A�v�A�VA�ffA��A���A�jA�A�A�$�A���A��
A�ZA��TA���A�$�A��-A�l�A�JA���A�  A��`A�S�A��!A���A��+A��A�~�A�r�A�S�A�1A�?}A���A�/A�~�A�~�A�?}A��A��+A��yA�hsA�A�%A�ĜA��\A�M�A�A�v�A�ȴA��uA��hA�VA�A��jA�7LA�v�A��/A��uA�&�A�oA�A�A�ĜA��`A��PA�G�A�A��A��-A���A���A�?}A�t�A��A�n�A��jA�bA}x�A|�yA{Az�Az-Az  Axv�AwdZAv~�At��Ar��Ar  AqK�Ao�An�!An{Am?}Al�Ah��Ag/Af�Ae�Ad��Ad�Ac�Aa��A`�A\v�AY"�AWhsAW/AW�AV�uAU�FAS"�AQ��AP��AO�ANQ�AL�+ALI�AKt�AI��AH�AG7LAFbAE��AE33AD�RADVAD  ACƨAC��AC?}A?�;A>�DA>I�A=�A="�A;G�A:z�A:1A9�;A9��A8��A8A7�A5�;A5K�A4bNA3�TA3oA1��A0(�A/�FA/O�A.�A-t�A-A,�+A,1A*VA)��A(jA'p�A&��A%p�A$JA"�yA"5?A�wA�9A`BA-A�^A/A�DA�7A�A�TA��Al�A$�AC�A�\A1A��A33A��A{A7LA��A�uAZAA�AbAG�A"�A�HA5?A
��A
$�A	A	/A9XA�`A�A�wAhsA$�A�DA-A�#A��A
=@��7@�V@�V@���@�A�@��@���@��@�-@���@�ȴ@�r�@�\)@�ȴ@�+@�v�@�^5@�V@�$�@��@���@�O�@�j@��@��#@�b@�V@�p�@�O�@�V@�1@���@��@�r�@۾w@ڏ\@��@�p�@ؼj@� �@׾w@֧�@�{@���@ӝ�@�S�@�o@�
=@ҸR@�=q@�7L@�;d@�%@�|�@ʰ!@��@�%@Ǯ@��@ư!@�^5@�$�@�@�1@å�@Ý�@�ȴ@�O�@�ƨ@��F@���@��@�+@��#@��@�G�@���@�z�@�Q�@��m@�o@�`B@�z�@�(�@��w@�"�@�n�@��@���@��m@�t�@�M�@�p�@��7@�7L@�Ĝ@�(�@�S�@�E�@�%@�bN@��;@�ƨ@���@�ȴ@�M�@�&�@��@��P@��+@���@�/@��@�I�@��@�ƨ@�dZ@�33@��@�V@��@��u@��@��@�;d@�33@��!@�M�@���@��h@��-@�X@�V@���@�r�@�Q�@�b@���@�t�@��@�~�@���@�&�@���@�Q�@� �@��@�
=@���@�O�@�&�@���@���@�A�@�1@���@��@��@��!@�~�@�ff@�5?@��@��^@��h@�X@��@��@���@�%@��^@��@��@�p�@�Ĝ@��
@���@�|�@���@���@��`@��@�  @�t�@�
=@��@��y@�v�@�{@���@���@�Ĝ@��D@��@��w@�|�@�K�@��y@��!@�v�@�ff@�^5@�5?@��T@���@��@���@�Ĝ@��/@���@���@�Ĝ@��@�9X@��P@�dZ@�o@�$�@��^@�p�@�?}@��@�V@���@��j@;d@
=@~ȴ@~��@~ff@~5?@~{@~@}@}��@}�h@}/@|�@|z�@{��@{�F@{t�@{S�@{S�@{S�@{@z^5@y��@yhs@x�9@x�u@x�@xA�@w��@v�R@v5?@u`B@t��@t�@t��@sƨ@r~�@q�#@q��@q�@pbN@pb@o��@o+@n�@nȴ@n�R@nv�@m�T@mO�@lz�@lI�@l�@k��@kC�@j��@j��@j�!@jn�@j�@i�@i�7@i7L@i�@i�@i&�@i%@h�u@h1'@g�@g�P@g;d@gK�@g+@f�@fȴ@fff@e@eO�@e�@eV@d��@dZ@c��@cƨ@c�F@ct�@c"�@c@b�@b��@a��@ahs@`Ĝ@`b@_�;@_��@_\)@_+@^��@^�@^��@^E�@^{@]��@]��@]�@\�@\1@[dZ@[@Z��@Z�\@ZJ@Y�#@Y�#@Y��@Y��@Y��@Y��@Y�#@Y�#@Y�#@Y�#@X�9@W�@W�@V��@VE�@U�-@T�j@T1@S�@R��@R-@Q��@Qx�@QX@QG�@Q&�@P��@P�`@P��@PĜ@P��@P1'@O|�@O;d@O+@N�y@Nȴ@N�R@N��@N��@N�+@Nff@NV@M�@MO�@L�j@L9X@L1@K�m@K�
@KdZ@J�@J~�@I�@Ihs@I7L@H�`@H�u@HbN@G�;@G|�@Gl�@G;d@F��@F�R@F�+@Fv�@Fff@FV@FE�@FE�@F{@E�T@E�-@D�@C�
@C�F@CS�@Co@B�@B��@B-@A�#@AG�@A&�@A%@@��@@�`@@�`@@�`@@Ĝ@?�;@?��@?;d@?�@>�@>ȴ@>�R@>V@>V@>5?@=�-@=/@<9X@;�
@;dZ@:��@:-@:J@9��@9��@9��@9��@9�@8A�@8 �@8 �@8 �@8b@8b@8b@8b@8  @7�;@7�w@7�@7��@7+@6�+@6@5�@5@5?}@4�@4z�@4I�@3�m@3��@3dZ@3"�@3@2~�@2^5@2=q@1��@0��@0��@0Q�@0b@/�;@/�@/��@/\)@.�R@.V@.5?@-��@-@-��@-�@-?}@-�@,�/@,z�@,Z@+�m@+ƨ@+�F@+��@+�@+dZ@+dZ@+C�@*�@*��@*�\@*~�@*~�@*~�@*~�@*~�@*~�@*~�@*~�@*n�@*^5@*^5@*=q@*=q@)�7@)&�@)�@(��@(��@(bN@(1'@'��@'\)@&�@&ȴ@&v�@&$�@%�@%O�@%V@$�/@$��@$j@$9X@$�@$1@#��@#�m@#�
@#�F@#��@#�@#o@"��@"��@"~�@"n�@"^5@"M�@"�@!�@!��@!&�@ ��@ Ĝ@ �9@ �u@ r�@ bN@ Q�@ A�@  �@   @�@�;@�w@�P@\)@ȴ@E�@�@�h@?}@V@�/@��@Z@9X@(�@�@1@1@��@�
@�F@t�@dZ@S�@33@"�@@�@��@�!@~�@^5@-@J@��@�7@x�@G�@�@��@ �@�;@�w@��@�P@+@�R@V@�@�-@��@�h@p�@/@�j@z�@�F@@�H@��@��@��@�\@~�@n�@M�@=q@=q@-@J@��@��@��@G�@&�@&�@��@��@�@r�@Q�@�;@|�@\)@;d@+@�@
=@��@�y@ȴ@�R@�+@V@{@�-@O�@?}@�@�/@��@�j@�D@1@��@t�@C�@33@33@33@"�@o@
�\@
^5@
M�@
M�@
M�@
=q@	�^@	��@	x�@	X@	X@	G�@	G�@	7L@	7L@	%@��@�u@bN@1'@b@  @�;@�@;d@�R@��@v�@V@5?@5?@@@@��@�h@�@�@`B@O�@?}@��@��@�@��@Z@1@��@�
@ƨ@��@33@o@@@@�@��@��@��@�!@~�@M�@�@��@hs@X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�ZB�NB�BB��B��B��B�#B�BB�TB�mB�sB�B�B�B��BBB��B��B  BBBB  BB  B��B��B��B�B��B��B�B�B�BB��B�B�B�yB�mB�fB�ZB�ZB�fB�TB�BB�B�
B�B�#B�#B��B�Bt�BM�BO�BS�BXB\)Be`BZB5?B
=BB��B��BhBhB1B  B�B�mB�BĜB�qB�RB�B��B�{B�1B^5BC�B?}B9XB2-B(�B�B�BbB
=B
��B
�B
�mB
�/B
ɺB
ĜB
�wB
�RB
�B
�B
��B
�hB
�B
y�B
iyB
[#B
O�B
9XB
49B
1'B
-B
,B
,B
&�B
�B
�B
VB
B	��B	��B	�B	�B	�mB	�HB	�B	��B	��B	�jB	�XB	�3B	�B	��B	��B	��B	�1B	s�B	gmB	e`B	ffB	cTB	^5B	N�B	E�B	D�B	@�B	:^B	2-B	/B	-B	$�B	�B	�B	{B	�B	�B	�B	�B	�B	�B	{B	hB	
=B	  B��B��B��B�B�B�B�B�yB�mB�TB�;B�#B�B��B��B��BǮB�}B�jB�^B�LB�-B�'B�!B�B��B��B�uB�PB�1B�B�B}�B}�Bw�Bs�Bo�BjBhsBgmBe`BaHB`BB]/B[#BXBT�BR�BP�BO�BN�BM�BL�BL�BJ�BJ�BI�BH�BH�BG�BF�BE�BD�BC�BE�BC�BC�BB�BA�BD�BC�BB�BA�BC�BB�BB�BB�BA�B@�BD�BF�BF�BF�BF�BF�BF�BF�BF�BG�BH�BI�BJ�BJ�BJ�BJ�BJ�BI�BI�BL�BL�BL�BL�BL�BN�BO�BQ�BQ�BQ�BQ�BR�BS�BVBW
BW
BYBYBZB\)B]/B]/B_;B_;BaHBdZBdZBe`Be`Be`BffBgmBk�Bq�Bt�Bu�Bw�Bx�Bx�Bx�Bz�B{�B{�B{�B�B�B�B�B�1B�DB�DB�DB�DB�JB�hB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�9B�?B�LB�^B�qB�qB�qB�qB�wB��BȴBɺB��B��B��B��B��B��B��B��B��B�B��B��B�
B�B�5B�BB�fB�B�B�B��B��B��B��B��B	  B	B	B	+B		7B	DB	PB	PB	\B	bB	hB	bB	bB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	%�B	'�B	(�B	)�B	+B	-B	.B	.B	2-B	8RB	<jB	>wB	B�B	G�B	H�B	I�B	J�B	J�B	J�B	I�B	H�B	H�B	J�B	M�B	P�B	Q�B	S�B	T�B	T�B	W
B	XB	YB	]/B	aHB	cTB	cTB	e`B	e`B	e`B	ffB	ffB	gmB	hsB	iyB	r�B	s�B	t�B	v�B	w�B	x�B	x�B	x�B	x�B	w�B	y�B	y�B	}�B	�B	�B	�%B	�+B	�+B	�+B	�+B	�JB	�PB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�!B	�!B	�'B	�'B	�9B	�FB	�?B	�?B	�FB	�FB	�LB	�LB	�LB	�RB	�XB	�jB	�qB	�qB	�wB	�}B	��B	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�BB	�NB	�NB	�NB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B
1B

=B

=B
DB
JB
JB
PB
PB
PB
VB
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
)�B
)�B
)�B
+B
,B
,B
-B
-B
-B
.B
.B
.B
/B
/B
/B
/B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
49B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
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
H�B
I�B
I�B
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
J�B
J�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
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
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
ZB
[#B
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
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
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
cTB
cTB
dZB
dZB
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
hsB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
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
t�B
t�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�ZB�NB�BB��B��B��B�#B�BB�TB�mB�sB�B�B�B��BBB��B��B  BBBB  BB  B��B��B��B�B��B��B�B�B�BB��B�B�B�yB�mB�fB�ZB�ZB�fB�TB�BB�B�
B�B�#B�#B��B�Bt�BM�BO�BS�BXB\)Be`BZB5?B
=BB��B��BhBhB1B  B�B�mB�BĜB�qB�RB�B��B�{B�1B^5BC�B?}B9XB2-B(�B�B�BbB
=B
��B
�B
�mB
�/B
ɺB
ĜB
�wB
�RB
�B
�B
��B
�hB
�B
y�B
iyB
[#B
O�B
9XB
49B
1'B
-B
,B
,B
&�B
�B
�B
VB
B	��B	��B	�B	�B	�mB	�HB	�B	��B	��B	�jB	�XB	�3B	�B	��B	��B	��B	�1B	s�B	gmB	e`B	ffB	cTB	^5B	N�B	E�B	D�B	@�B	:^B	2-B	/B	-B	$�B	�B	�B	{B	�B	�B	�B	�B	�B	�B	{B	hB	
=B	  B��B��B��B�B�B�B�B�yB�mB�TB�;B�#B�B��B��B��BǮB�}B�jB�^B�LB�-B�'B�!B�B��B��B�uB�PB�1B�B�B}�B}�Bw�Bs�Bo�BjBhsBgmBe`BaHB`BB]/B[#BXBT�BR�BP�BO�BN�BM�BL�BL�BJ�BJ�BI�BH�BH�BG�BF�BE�BD�BC�BE�BC�BC�BB�BA�BD�BC�BB�BA�BC�BB�BB�BB�BA�B@�BD�BF�BF�BF�BF�BF�BF�BF�BF�BG�BH�BI�BJ�BJ�BJ�BJ�BJ�BI�BI�BL�BL�BL�BL�BL�BN�BO�BQ�BQ�BQ�BQ�BR�BS�BVBW
BW
BYBYBZB\)B]/B]/B_;B_;BaHBdZBdZBe`Be`Be`BffBgmBk�Bq�Bt�Bu�Bw�Bx�Bx�Bx�Bz�B{�B{�B{�B�B�B�B�B�1B�DB�DB�DB�DB�JB�hB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�9B�?B�LB�^B�qB�qB�qB�qB�wB��BȴBɺB��B��B��B��B��B��B��B��B��B�B��B��B�
B�B�5B�BB�fB�B�B�B��B��B��B��B��B	  B	B	B	+B		7B	DB	PB	PB	\B	bB	hB	bB	bB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	%�B	'�B	(�B	)�B	+B	-B	.B	.B	2-B	8RB	<jB	>wB	B�B	G�B	H�B	I�B	J�B	J�B	J�B	I�B	H�B	H�B	J�B	M�B	P�B	Q�B	S�B	T�B	T�B	W
B	XB	YB	]/B	aHB	cTB	cTB	e`B	e`B	e`B	ffB	ffB	gmB	hsB	iyB	r�B	s�B	t�B	v�B	w�B	x�B	x�B	x�B	x�B	w�B	y�B	y�B	}�B	�B	�B	�%B	�+B	�+B	�+B	�+B	�JB	�PB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�!B	�!B	�'B	�'B	�9B	�FB	�?B	�?B	�FB	�FB	�LB	�LB	�LB	�RB	�XB	�jB	�qB	�qB	�wB	�}B	��B	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�BB	�NB	�NB	�NB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
	7B
1B

=B

=B
DB
JB
JB
PB
PB
PB
VB
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
)�B
)�B
)�B
+B
,B
,B
-B
-B
-B
.B
.B
.B
/B
/B
/B
/B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
49B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
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
H�B
I�B
I�B
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
J�B
J�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
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
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
ZB
[#B
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
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
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
cTB
cTB
dZB
dZB
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
hsB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
iyB
iyB
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
t�B
t�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.21 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230404110108                              AO  ARCAADJP                                                                    20230404110108    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230404110108  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230404110108  QCF$                G�O�G�O�G�O�0               