CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-03-18T17:02:13Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
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
resolution        =���   axis      Z        `  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  o@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �x   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �H   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ǩ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ˀ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �        �Argo profile    3.1 1.2 19500101000000  20180318170213  20181023151223  4901546 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4737                            2C  D   NAVIS_A                         0171                            120111                          863 @�T]��1   @�T]���2@<��hr��c��S���1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@x��@�G�@�G�A��A<��A\��A~=qA�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�BB(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dl)D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;��D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�Dur�Du�Dvr�Dv�Dwr�Dw�Dxr�Dx�Dyr�Dy�Dzr�Dz�D{r�D{�D|r�D|�D}r�D}�D~r�D~�Dr�D�D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD¹HD��HD�9HD�yHDùHD��HD�9HD�yHDĹHD��HD�9HD�yHDŹHD��HD�9HD�yHDƹHD��HD�9HD�yHDǹHD��HD�9HD�yHDȹHD��HD�9HD�yHDɹHD��HD�9HD�yHDʹHD��HD�9HD�yHD˹HD��HD�9HD�yHD̹HD��HD�9HD�yHD͹HD��HD�9HD�yHDιHD��HD�9HD�yHDϹHD��HD�9HD�yHDйHD��HD�9HD�yHDѹHD��HD�9HD�yHDҹHD��HD�9HD�yHDӹHD��HD�9HD�yHDԹHD��HD�9HD�yHDչHD��HD�9HD�yHDֹHD��HD�9HD�yHD׹HD��HD�9HD�yHDعHD��HD�9HD�yHDٹHD��HD�9HD�yHDڹHD��HD�9HD�yHD۹HD��HD�9HD�yHDܹHD��HD�9HD�yHDݹHD��HD�9HD�yHD޹HD��HD�9HD�yHD߹HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��{D�,{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�ĜA�ƨA�ƨA�ƨA���A��9A�A���A���A���A�ƨA�ƨA��jA��^A�n�A��
A�bNA�XA���A���A���A��FA�t�A�M�A��A��hA�jA��mA���A�ZA���A��yA�r�A��!A��A��jA�r�A��A��mA�VA�ȴA��9A�/A�I�A�;dA��A���A�ƨA��\A��7A��A��A�t�A�bNA�E�A� �A��A��yA���A�hsA��A�C�A�-A��A�^5A�ĜA�K�A��A�ƨA�M�A���A�O�A�bA��hA��A��`A�bA��#A��A�`BA�%A��A~��A~(�A}p�Az�!Axz�Aw�FAv��Aut�Au%As�hAp��Ap�\Ao�#Ann�Al�Ak\)Ai�7Ah�Ah1Ae��Adv�Ab~�A`�RA^�A]�-A\��A\1A[S�A[&�AZ��AY�AX��AX�AX��AX�!AWG�AU�AT��ATZAS�AS�wAS�ASVAR��AR~�AQ�APn�AO�#AO��AO��AN�AN$�AM��AL~�AK/AJbNAJ1AH�/AGx�AF�RAFM�AFE�AFE�AFr�AF1'AEx�ADJACdZAC
=AB�uAB9XAA��AA��AAhsAAG�AA�A@ĜA@5?A?ƨA?�wA?�wA?|�A>��A<��A<=qA;��A;?}A;%A:�yA:��A9�A8��A7O�A65?A5��A5K�A4��A4  A2�yA25?A2$�A2�A2A1ƨA1p�A0r�A/�hA.ffA-x�A,ȴA,��A+ƨA)��A(��A'p�A&$�A%��A$=qA#��A#oA!7LA��AC�A�yA�A�`A�\A�^Ap�A`BA&�A��A�AXA�A%AȴAK�A�+A�TA�AJAoA �A�AXA�`A�-A"�A��A  A
�yA	�#A	��A	?}An�A�TA|�A�yA�\A��A�DA�A��A ��A �@��@�7L@�Ĝ@�Z@�l�@���@�b@���@�{@��@�  @�\)@�M�@�%@�A�@��@�7@���@�u@��@�ff@�1@�hs@⟾@��@�G�@�&�@��@�I�@��@�K�@���@��@�;d@ְ!@��@ҟ�@���@��m@υ@�=q@�O�@̬@˕�@��H@�{@Ȭ@�K�@�E�@�J@�@Ĭ@��;@�l�@�
=@¸R@�^5@�5?@�-@��7@���@���@�o@��@��h@�7L@���@��9@��@���@�ƨ@���@�l�@���@���@��
@�v�@���@�&�@�Z@���@�K�@�
=@���@��-@�x�@�hs@���@�bN@�Q�@���@�
=@��T@��7@���@�1@��P@�$�@�x�@�&�@��@�Ĝ@���@��\@�J@�O�@���@�bN@�|�@��R@�@���@�X@�  @���@��@��@�5?@���@�%@�b@�ff@�5?@�$�@���@���@��@�Q�@��
@�l�@�C�@�
=@��@���@�hs@�/@�%@���@��@�1'@��w@��P@�l�@���@���@�E�@��h@�X@�?}@�?}@�&�@��@���@��j@�j@��
@��w@�|�@�33@��R@��\@�v�@�V@�{@��7@�%@��@��D@��@��@��F@�t�@�"�@���@�n�@�-@��@��-@���@��@��@�hs@��@���@��`@���@�I�@��@|�@+@~�R@}�@}�@}?}@|�@|�@|�@|�@|�j@|�D@|I�@{�m@{��@{��@{t�@{S�@{33@{o@z��@z��@z=q@zJ@y�#@y��@yhs@y7L@x�9@x  @wK�@v$�@u��@up�@u�@t�j@t9X@s�F@s��@s��@s�@sS�@s@r=q@q��@q�@p�u@p�@pA�@o��@o\)@nȴ@n�+@nv�@nV@n5?@m?}@lz�@lI�@l1@kƨ@kS�@j�H@j��@j^5@i��@ix�@i7L@h�`@h��@hbN@h�@h�9@h�@h �@g�P@f�@f5?@e@ep�@e?}@e/@d��@d�@dz�@d9X@d1@c�m@cƨ@c�F@c�@c"�@b��@b^5@a��@a�7@a��@ahs@a%@`��@`�9@`Q�@_�w@_�P@_
=@^��@^�y@^�R@^��@^v�@^5?@^$�@^{@]�@]�-@]��@\��@\9X@[�m@[�
@[��@[��@[t�@[S�@[@Z�H@Z��@Zn�@Z=q@Z�@Y�@Yhs@Y%@X�`@X�9@XbN@W�@W|�@W\)@WK�@W�@V��@V��@V�+@VE�@VE�@U@U?}@T�@T�@T�@Tj@St�@R��@R�\@Q�#@Q7L@P��@Pr�@P1'@O�@Ol�@O
=@N�y@Nȴ@N�R@Nff@N$�@M�T@M�h@M?}@L�@L�@L�D@Lz�@L��@L�@L�D@Lj@Lj@LI�@LI�@LI�@K�m@KS�@J~�@I��@I�7@IG�@I%@HĜ@Hr�@G��@F�y@F��@F�+@Fff@F@E��@E�h@E?}@D��@D�/@D�j@Dj@D�@C�m@C�F@C��@C��@C�@CS�@CC�@CC�@CS�@CS�@CC�@CC�@CC�@C33@B��@B�@A��@A��@AG�@A�@@��@@��@@�@@bN@@A�@@ �@?�;@?l�@?;d@>��@>��@>E�@>@=�T@=@=�-@=�@<��@<j@<9X@<�@<1@;�m@;�
@;ƨ@;o@:��@:~�@:^5@:M�@:-@:�@9�@9�7@9&�@8��@7��@7\)@7+@7
=@7
=@6��@6�y@6�y@6�@6��@6$�@5�@5�-@5�-@5�-@5�-@5`B@5O�@4�/@49X@3�m@3��@3t�@3o@2��@2=q@1��@1�7@1x�@1x�@1X@0�`@0��@0Q�@/�@/�w@/��@/|�@/+@.ȴ@.ff@.{@-��@-�@-?}@-�@,��@,��@,z�@,j@,Z@,9X@,(�@+��@+�F@+dZ@+S�@+C�@*��@*�\@*~�@*^5@*=q@*-@*-@)��@)�^@)hs@)�@)%@)%@(�`@(Ĝ@(��@(r�@(Q�@(A�@( �@(  @'�@'\)@';d@'�@&�@&�@&�@&�@&ȴ@&ȴ@&�R@&��@&5?@%�@%�@%�T@%�T@%@%�h@%�@$��@$�@$9X@#�m@#ƨ@#�@#S�@#o@"��@"��@"��@"��@"�!@"�\@"=q@!�@!��@!x�@!X@!7L@!%@ �`@ bN@  �@�@��@�w@�@��@��@�P@K�@;d@
=@ȴ@�+@5?@@��@�h@�@�/@�@z�@Z@I�@(�@�@�@1@1@�F@S�@o@�@��@�\@^5@M�@-@��@�#@�^@x�@x�@x�@hs@X@G�@%@��@�`@Ĝ@�9@��@�@�@bN@A�@�@�@��@|�@K�@
=@�R@v�@@@�@�T@�h@O�@�@�@V@�/@��@�j@�D@(�@�m@��@�@dZ@C�@@�!@�\@M�@�@�#@��@x�@G�@7L@&�@�@�@%@��@��@�9@�u@Q�@ �@�@�w@�P@|�@l�@�@
=@�y@�y@�@��@5?@�@�T@��@��@�@`B@�@�j@��@Z@9X@�@��@ƨ@�F@��@��@�@t�@dZ@o@
��@
��@
~�@
n�@
M�@
-@
J@	�@	��@	�^@	��@	�7@	x�@	G�@	&�@	%@��@��@Ĝ@Ĝ@��@�@r�@r�@bN@1'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A�A�ĜA�ƨA�ƨA�ƨA���A��9A�A���A���A���A�ƨA�ƨA��jA��^A�n�A��
A�bNA�XA���A���A���A��FA�t�A�M�A��A��hA�jA��mA���A�ZA���A��yA�r�A��!A��A��jA�r�A��A��mA�VA�ȴA��9A�/A�I�A�;dA��A���A�ƨA��\A��7A��A��A�t�A�bNA�E�A� �A��A��yA���A�hsA��A�C�A�-A��A�^5A�ĜA�K�A��A�ƨA�M�A���A�O�A�bA��hA��A��`A�bA��#A��A�`BA�%A��A~��A~(�A}p�Az�!Axz�Aw�FAv��Aut�Au%As�hAp��Ap�\Ao�#Ann�Al�Ak\)Ai�7Ah�Ah1Ae��Adv�Ab~�A`�RA^�A]�-A\��A\1A[S�A[&�AZ��AY�AX��AX�AX��AX�!AWG�AU�AT��ATZAS�AS�wAS�ASVAR��AR~�AQ�APn�AO�#AO��AO��AN�AN$�AM��AL~�AK/AJbNAJ1AH�/AGx�AF�RAFM�AFE�AFE�AFr�AF1'AEx�ADJACdZAC
=AB�uAB9XAA��AA��AAhsAAG�AA�A@ĜA@5?A?ƨA?�wA?�wA?|�A>��A<��A<=qA;��A;?}A;%A:�yA:��A9�A8��A7O�A65?A5��A5K�A4��A4  A2�yA25?A2$�A2�A2A1ƨA1p�A0r�A/�hA.ffA-x�A,ȴA,��A+ƨA)��A(��A'p�A&$�A%��A$=qA#��A#oA!7LA��AC�A�yA�A�`A�\A�^Ap�A`BA&�A��A�AXA�A%AȴAK�A�+A�TA�AJAoA �A�AXA�`A�-A"�A��A  A
�yA	�#A	��A	?}An�A�TA|�A�yA�\A��A�DA�A��A ��A �@��@�7L@�Ĝ@�Z@�l�@���@�b@���@�{@��@�  @�\)@�M�@�%@�A�@��@�7@���@�u@��@�ff@�1@�hs@⟾@��@�G�@�&�@��@�I�@��@�K�@���@��@�;d@ְ!@��@ҟ�@���@��m@υ@�=q@�O�@̬@˕�@��H@�{@Ȭ@�K�@�E�@�J@�@Ĭ@��;@�l�@�
=@¸R@�^5@�5?@�-@��7@���@���@�o@��@��h@�7L@���@��9@��@���@�ƨ@���@�l�@���@���@��
@�v�@���@�&�@�Z@���@�K�@�
=@���@��-@�x�@�hs@���@�bN@�Q�@���@�
=@��T@��7@���@�1@��P@�$�@�x�@�&�@��@�Ĝ@���@��\@�J@�O�@���@�bN@�|�@��R@�@���@�X@�  @���@��@��@�5?@���@�%@�b@�ff@�5?@�$�@���@���@��@�Q�@��
@�l�@�C�@�
=@��@���@�hs@�/@�%@���@��@�1'@��w@��P@�l�@���@���@�E�@��h@�X@�?}@�?}@�&�@��@���@��j@�j@��
@��w@�|�@�33@��R@��\@�v�@�V@�{@��7@�%@��@��D@��@��@��F@�t�@�"�@���@�n�@�-@��@��-@���@��@��@�hs@��@���@��`@���@�I�@��@|�@+@~�R@}�@}�@}?}@|�@|�@|�@|�@|�j@|�D@|I�@{�m@{��@{��@{t�@{S�@{33@{o@z��@z��@z=q@zJ@y�#@y��@yhs@y7L@x�9@x  @wK�@v$�@u��@up�@u�@t�j@t9X@s�F@s��@s��@s�@sS�@s@r=q@q��@q�@p�u@p�@pA�@o��@o\)@nȴ@n�+@nv�@nV@n5?@m?}@lz�@lI�@l1@kƨ@kS�@j�H@j��@j^5@i��@ix�@i7L@h�`@h��@hbN@h�@h�9@h�@h �@g�P@f�@f5?@e@ep�@e?}@e/@d��@d�@dz�@d9X@d1@c�m@cƨ@c�F@c�@c"�@b��@b^5@a��@a�7@a��@ahs@a%@`��@`�9@`Q�@_�w@_�P@_
=@^��@^�y@^�R@^��@^v�@^5?@^$�@^{@]�@]�-@]��@\��@\9X@[�m@[�
@[��@[��@[t�@[S�@[@Z�H@Z��@Zn�@Z=q@Z�@Y�@Yhs@Y%@X�`@X�9@XbN@W�@W|�@W\)@WK�@W�@V��@V��@V�+@VE�@VE�@U@U?}@T�@T�@T�@Tj@St�@R��@R�\@Q�#@Q7L@P��@Pr�@P1'@O�@Ol�@O
=@N�y@Nȴ@N�R@Nff@N$�@M�T@M�h@M?}@L�@L�@L�D@Lz�@L��@L�@L�D@Lj@Lj@LI�@LI�@LI�@K�m@KS�@J~�@I��@I�7@IG�@I%@HĜ@Hr�@G��@F�y@F��@F�+@Fff@F@E��@E�h@E?}@D��@D�/@D�j@Dj@D�@C�m@C�F@C��@C��@C�@CS�@CC�@CC�@CS�@CS�@CC�@CC�@CC�@C33@B��@B�@A��@A��@AG�@A�@@��@@��@@�@@bN@@A�@@ �@?�;@?l�@?;d@>��@>��@>E�@>@=�T@=@=�-@=�@<��@<j@<9X@<�@<1@;�m@;�
@;ƨ@;o@:��@:~�@:^5@:M�@:-@:�@9�@9�7@9&�@8��@7��@7\)@7+@7
=@7
=@6��@6�y@6�y@6�@6��@6$�@5�@5�-@5�-@5�-@5�-@5`B@5O�@4�/@49X@3�m@3��@3t�@3o@2��@2=q@1��@1�7@1x�@1x�@1X@0�`@0��@0Q�@/�@/�w@/��@/|�@/+@.ȴ@.ff@.{@-��@-�@-?}@-�@,��@,��@,z�@,j@,Z@,9X@,(�@+��@+�F@+dZ@+S�@+C�@*��@*�\@*~�@*^5@*=q@*-@*-@)��@)�^@)hs@)�@)%@)%@(�`@(Ĝ@(��@(r�@(Q�@(A�@( �@(  @'�@'\)@';d@'�@&�@&�@&�@&�@&ȴ@&ȴ@&�R@&��@&5?@%�@%�@%�T@%�T@%@%�h@%�@$��@$�@$9X@#�m@#ƨ@#�@#S�@#o@"��@"��@"��@"��@"�!@"�\@"=q@!�@!��@!x�@!X@!7L@!%@ �`@ bN@  �@�@��@�w@�@��@��@�P@K�@;d@
=@ȴ@�+@5?@@��@�h@�@�/@�@z�@Z@I�@(�@�@�@1@1@�F@S�@o@�@��@�\@^5@M�@-@��@�#@�^@x�@x�@x�@hs@X@G�@%@��@�`@Ĝ@�9@��@�@�@bN@A�@�@�@��@|�@K�@
=@�R@v�@@@�@�T@�h@O�@�@�@V@�/@��@�j@�D@(�@�m@��@�@dZ@C�@@�!@�\@M�@�@�#@��@x�@G�@7L@&�@�@�@%@��@��@�9@�u@Q�@ �@�@�w@�P@|�@l�@�@
=@�y@�y@�@��@5?@�@�T@��@��@�@`B@�@�j@��@Z@9X@�@��@ƨ@�F@��@��@�@t�@dZ@o@
��@
��@
~�@
n�@
M�@
-@
J@	�@	��@	�^@	��@	�7@	x�@	G�@	&�@	%@��@��@Ĝ@Ĝ@��@�@r�@r�@bN@1'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�PB|�Bp�BaHBn�Bv�Br�Bn�Bm�Bp�BhsBT�BM�BJ�BC�BA�B5?B!�B�BhBDBB��B�B�HBǮB�RB��B� Bz�Bv�Bp�BjBiyBhsBiyBiyBhsBe`BaHB\)B[#BYBS�BD�B<jB;dB:^B/B
��B
�B
�B
�yB
�;B
�B
��B
�9B
�B
��B
��B
�JB
�B
�B
|�B
|�B
�B
�B
~�B
w�B
dZB
R�B
L�B
E�B
=qB
9XB
0!B
 �B
�B
�B
\B
%B	��B	�B	�B	�mB	�
B	��B	��B	�LB	�'B	�'B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�\B	�JB	�7B	�+B	�%B	�B	�B	�B	{�B	{�B	u�B	r�B	q�B	p�B	l�B	hsB	ffB	_;B	W
B	P�B	M�B	G�B	@�B	=qB	<jB	<jB	;dB	<jB	:^B	6FB	0!B	,B	)�B	'�B	%�B	'�B	'�B	&�B	%�B	$�B	"�B	�B	�B	�B	�B	�B	�B	JB		7B	%B	B	B	B	B��B��B�B�yB�fB�ZB�HB�/B�B��B��B��B��B��B��B��BȴBÖB�}B�jB�dB�FB�B��B��B��B��B��B��B�hB�=B�+B�B�B�B}�B|�Bz�By�Bx�Bw�Bu�Bs�Br�Bq�Bp�Bn�BjBhsBffBdZBaHB_;B\)B[#BZBXBT�BQ�BM�BJ�BH�BG�BF�BE�BE�BE�BE�BD�BC�BA�B?}B<jB;dB9XB8RB6FB6FB5?B49B1'B-B+B)�B(�B'�B&�B%�B#�B"�B!�B �B!�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B$�B$�B(�B&�B%�B$�B%�B$�B$�B#�B#�B#�B#�B#�B%�B&�B(�B)�B+B+B,B-B1'B5?B7LB:^B;dB<jB<jB;dB;dB<jB<jB=qB=qBA�BD�BD�BD�BF�BF�BG�BF�BC�BB�BB�BC�BG�BH�BI�BI�BM�BL�BI�BH�BI�BJ�BI�BH�BJ�BL�BO�BR�BT�B[#B^5B^5B`BB`BBbNBe`BgmBiyBhsBhsBhsBk�Bl�Bm�Bn�Bo�Bp�Bt�Bx�By�By�Bz�B{�B~�B�B�%B�DB�JB�VB��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�?B�?B�FB�FB�LB�LB�LB�^B�qBÖBĜBȴB��B��B��B��B��B��B�B�#B�5B�;B�;B�TB�fB�sB�B�B�B��B��B��B��B��B��B��B��B��B	  B	B	B	1B		7B	DB	PB	\B	hB	oB	uB	uB	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	"�B	$�B	%�B	&�B	'�B	(�B	-B	0!B	33B	7LB	8RB	:^B	<jB	>wB	@�B	C�B	C�B	C�B	D�B	D�B	E�B	H�B	K�B	N�B	P�B	P�B	Q�B	S�B	VB	XB	ZB	ZB	\)B	^5B	`BB	dZB	ffB	iyB	k�B	l�B	m�B	n�B	o�B	o�B	q�B	r�B	t�B	v�B	w�B	y�B	z�B	{�B	{�B	|�B	~�B	�B	�B	�B	�+B	�+B	�1B	�=B	�=B	�JB	�VB	�VB	�\B	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�-B	�3B	�3B	�9B	�9B	�?B	�FB	�LB	�RB	�XB	�^B	�^B	�jB	�qB	�wB	�}B	��B	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�)B	�/B	�5B	�BB	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
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
PB
PB
VB
\B
\B
bB
hB
oB
oB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
(�B
)�B
+B
+B
+B
+B
,B
-B
.B
.B
/B
/B
/B
/B
0!B
1'B
1'B
2-B
2-B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
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
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
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
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
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
S�B
S�B
S�B
T�B
T�B
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
YB
ZB
ZB
ZB
ZB
ZB
ZB
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
^5B
^5B
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
`BB
`BB
aHB
aHB
aHB
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
cTB
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
hsB
iyB
iyB
iyB
iyB
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
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�tB��B�6Bu6BaqBn(Bw�BszBo�BpLBvQBonBY�BP�BL�BDBDB9)B$�B�BBFBSBB��B�PB��B��B�:B��B{�Bw�Bq�Bj�Bi�Bh�Bi�Bi�Bi Bf/BbGB\jB[�B[wB[BG�B<�B;�B=JB8B �B
�!B
�FB
�\B
�B
��B
�B
��B
��B
��B
�tB
��B
��B
��B
~�B
~#B
��B
�+B
�B
~�B
i�B
U B
OvB
IB
>�B
=:B
6�B
"B
�B
B
�B

B
UB	�#B	��B	�B	�"B	�pB	��B	��B	�"B	�B	��B	��B	�sB	�vB	��B	�YB	��B	��B	��B	�!B	��B	��B	��B	�_B	��B	��B	�WB	��B	�B	}�B	�B	w0B	s-B	q�B	r$B	m�B	iFB	iB	a�B	X�B	Q�B	PcB	J�B	B-B	>dB	<�B	<�B	;B	=B	<B	9�B	1�B	,�B	+/B	(�B	&�B	(�B	(�B	'PB	&aB	%�B	$@B	 �B	�B	�B	lB	�B	�B	B	
�B	DB	�B	tB	rB	=B�gB��B�lB��B�SB�B�_B��B��B�AB�6B�KBԡB��B�gB�B˂B��B�7B��B��B��B�B��B�B��B��B�B�B�B��B��B�-B��B��B~�B
B{�Bz"By�ByVBw�Bu&BsoBq�BqzBrWBl�BjLBh�Bg2BdBa�B]yB\+B[xB[@BV�BV1BRJBM�BK�BHqBG�BG�BG:BF�BGIBE�BE�BE�BBzB@�B?B;B:DB8�B7B6 B5�B3�B/�B-B*�B*�B)`B'�B'�B%�B$B#�B"�B"�B"�B"YB"�B"UB �B�BB#B�B�B�BB�BZBB�BdB�B�B�B# B%tB&�B*7B'�B'WB%�B'B&�B&�B%KB$>B$cB%jB%B&�B'�B)�B*�B+WB+/B-)B/nB1�B6'B8�B;NB<B<�B=B<�B?"B?}B<�B=�B?BDTBF�BF�BFBG�BHMBM�BH�BD BCiBDBD	BG�BI�BJ�BI�BN�BNWBK�BIlBKBLBJ�BJ�BK�BMaBPBS�BV�B\�B_B_[B`�BaBc�BfkBh[Bi�Bh�Bj"Bi�Bk�Bl�Bn�BorBpuBq�Bv�By-BzBz.B{tB|�B�B��B��B��B��B��B�IB��B�
B�B�B�?B�OB��B�RB�RB�B�^B��B�MB��B�{B�`B�zB�zB��B��B��B�6B��B�B�'B�lB�+B�#B�<B�gB��B��B۬B�zB�bB��B��B��B��B�"B�B�B� B�7B�B�B�B�B�aB�;B�5B	 nB	�B	�B	B		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 B	! B	!B	#B	%B	&,B	'(B	(.B	)gB	-�B	0�B	4B	7�B	8�B	:�B	<�B	>�B	@�B	C�B	C�B	C�B	D�B	D�B	FGB	IAB	LVB	ORB	QB	Q5B	R]B	TkB	V�B	XYB	ZEB	ZLB	\WB	^�B	`�B	d�B	f�B	i�B	k�B	l�B	m�B	n�B	o�B	pB	q�B	r�B	uB	wB	w�B	y�B	{B	|@B	|eB	}�B	�B	�vB	�iB	�\B	�RB	�iB	�~B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	�0B	�SB	� B	�QB	�B	�B	�*B	�"B	�$B	�:B	�B	�&B	�-B	�GB	�-B	��B	��B	�uB	�TB	�lB	�[B	�fB	�nB	��B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�#B	�*B	�B	�B	��B	�'B	ϜB	�oB	�FB	֖B	ؚB	ژB	�dB	�mB	�wB	ިB	��B	�B	�B	�{B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	�B	�B	�B	�0B	��B	�B	��B	��B	�B	�B	�JB	�rB	�&B	�	B	�B	�;B	�&B	�7B	�JB	�@B
 .B
 3B
TB
WB
GB
HB
DB
3B
:B
TB
AB
2B
.B
:B
EB
<B
<B
KB
�B
�B
	�B

eB

�B
�B
yB
�B
~B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
#B
�B
�B
�B
�B
�B
�B
�B
 B
B
!B
YB
!B
!B
 �B
 �B
 �B
 �B
!�B
!�B
"	B
";B
#B
$!B
#�B
#�B
#�B
$,B
%B
%MB
%sB
&8B
&0B
',B
'LB
(IB
)pB
*�B
+4B
+,B
+B
+:B
,{B
-\B
.pB
.zB
/\B
/PB
/PB
/uB
0�B
1�B
1�B
2}B
2�B
3�B
4oB
4�B
4tB
5nB
5dB
5gB
5pB
5dB
5zB
5�B
6�B
7qB
7pB
7�B
8�B
9{B
9�B
9�B
9}B
9wB
9�B
:�B
:�B
;�B
;�B
;~B
;�B
;�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
GB
F�B
G�B
G�B
G�B
H�B
H�B
I7B
JB
I�B
J�B
J�B
J�B
J�B
J�B
J�B
KB
K�B
L
B
LB
LB
M)B
MB
NB
N$B
NHB
O)B
OB
P!B
PB
PB
PB
PB
QB
QB
QB
Q@B
QNB
Q6B
R#B
R2B
R/B
S5B
SB
S'B
S7B
S-B
S+B
THB
TB
TB
T%B
TB
T#B
TIB
U'B
U)B
U4B
U)B
U,B
U6B
V"B
V@B
V=B
VcB
V"B
VBB
VgB
WPB
WaB
WlB
XgB
X�B
Y1B
YAB
YCB
YzB
YnB
Y[B
Z7B
ZHB
ZbB
ZDB
ZHB
ZgB
[�B
\}B
\}B
\cB
\cB
\gB
\B
\�B
]iB
]�B
^~B
^�B
_�B
_�B
_�B
_hB
_fB
`kB
`]B
`kB
`mB
`B
`{B
`{B
a�B
a�B
a�B
b�B
b�B
byB
b}B
b�B
c�B
c�B
cqB
c�B
c�B
c�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
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
h�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
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
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%X]<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.21 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201810180929272018101809292720181018092927  AO  ARCAADJP                                                                    20180318170213    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180318170213  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180318170213  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20181018092927  QC  PRES            @�33D�33G�O�                PM  ARSQCTM V1.1                                                                20181018092927  QC  PSAL            @�33D�33G�O�                PM  ARSQOWGUV1.0CTD_2017v1 + Argo_2017v02                                       20181023151223  IP                  G�O�G�O�G�O�                