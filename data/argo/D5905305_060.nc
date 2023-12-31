CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-05-11T11:00:29Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pX   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʌ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �X   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �X   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �X   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �X   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �        �Argo profile    3.1 1.2 19500101000000  20190511110029  20200901151524  5905305 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               <A   AO  6986                            2C  D   NAVIS_A                         0833                            170425                          863 @ؽ�V��1   @ؽI���@1��C���d�A�7K�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      <A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�	�D�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@\(@�G�@�G�A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)��C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�Dur�Du�Dvr�Dv�Dwr�Dw�Dxr�Dx�Dyr�Dy�Dzr�Dz�D{r�D{�D|r�D|�D}r�D}�D~r�D~�Dr�D�D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD¹HD��HD�9HD�yHDùHD��HD�9HD�yHDĹHD��HD�9HD�yHDŹHD��HD�9HD�yHDƹHD��HD�9HD�yHDǹHD��HD�9HD�yHDȹHD��HD�9HD�yHDɹHD��HD�9HD�yHDʹHD��HD�9HD�yHD˹HD��HD�9HD�yHD̹HD��HD�9HD�yHD͹HD��HD�9HD�yHDιHD��HD�9HD�yHDϹHD��HD�9HD�yHDйHD��HD�9HD�yHDѹHD��HD�9HD�yHDҹHD��HD�9HD�yHDӹHD��HD�9HD�yHDԹHD��HD�9HD�yHDչHD��HD�9HD�yHDֹHD��HD�9HD�yHD׹HD��HD�9HD�yHDعHD��HD�9HD�yHDٹHD��HD�9HD�yHDڹHD��HD�9HD�yHD۹HD��HD�9HD�yHDܹHD��HD�9HD�yHDݹHD��HD�9HD�yHD޹HD��HD�9HD�yHD߹HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�D��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�<{D�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��D�"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AՋDAՍPAՏ\AՑhAՏ\AՍPAՋDAՅA�z�A�\)A��AԅA�bA�C�A���Aѩ�A�r�A��A��A���AГuA�bNA��A�5?A�ƨA΍PA�t�A�E�A��yAͼjA�ffA̸RA��Aə�Aȥ�A�t�AƍPA���A�7LA�bNA��A�=qA�\)A���A��^A��A��TA�;dA�7LA���A��A�$�A�A�7LA��A�\)A��A���A���A�5?A�"�A��A�5?A�"�A�=qA�"�A�E�A�+A�x�A��A�dZA�|�A�-A�hsA�dZA��-A�I�A��A��A�|�A���A���A��A�5?A��FA�;dA���A�;dA�%A�/A�&�A��7A���A��A;dA~Az��Av�Ar{Aq�AqS�Ap�Am��Ae&�A`�RA]p�A[�-AY��AX �AV9XAS�;AQ�AP�+AN��AL�uAJ�!AI��AIt�AIG�AH��AHbNAF��AE�AD��ADn�AC�
AB^5A?�^A=;dA;��A;&�A8I�A5�A3��A1x�A/dZA-�A,M�A+C�A)�
A)�A(��A(5?A'A't�A'33A&��A&�A$�HA#��A#l�A#G�A#+A#
=A"�jA!�mA ��A Q�A $�A�PA(�AoA��A��A�A�jA\)A�A�RA��A��A�wA��A1A  A��A+AbA��A�A�A&�A	�A	7LA~�At�A�RAƨA��AJA��AA�\A��A��A ��@��F@��+@���@�
=@���@�=q@�x�@���@��@�@�P@�C�@�"�@�ȴ@�V@�A�@�  @�|�@�K�@�@�@��@땁@�V@�  @��@�\@��#@��@�9X@�P@��@⟾@�ff@���@���@�ƨ@ߥ�@ߥ�@�33@�=q@�O�@�Ĝ@۾w@�|�@�"�@�v�@٩�@�7L@�%@���@��@ؓu@ו�@�ȴ@��T@ԓu@�@�ff@�=q@��@�`B@ϕ�@��y@�n�@�J@��@�hs@��/@�Z@˝�@�o@ʟ�@�V@��T@�hs@�j@��@�@��T@Ł@�/@���@ŉ7@�G�@��
@§�@�+@�J@��^@���@� �@���@�S�@�K�@�
=@���@���@��@�7L@�p�@���@�hs@�p�@��@���@��D@�j@�bN@�b@�dZ@��+@��@��-@���@�9X@�9X@���@�ȴ@�$�@�-@�-@�$�@��@�{@�{@���@��#@���@���@��h@��@�hs@��@���@���@���@��;@�33@���@�^5@�@��@��-@��/@��j@���@���@���@���@���@�j@�9X@�K�@���@�v�@�=q@�M�@��^@�?}@�7L@�/@��@���@�%@���@���@�9X@�(�@���@��;@��F@�@�7L@�%@��`@��9@��D@�bN@� �@�1@��m@���@���@��R@��@�x�@�Ĝ@�z�@�I�@��@���@�;d@�@��R@�ff@�@��@�Q�@�Q�@�b@��@�l�@��H@���@�E�@�-@���@���@�O�@��`@��@��@���@��@�V@��^@��@�?}@�%@��/@��j@���@��@�Z@�9X@�b@�1@���@�C�@��@���@�^5@���@�x�@�`B@��@�I�@��;@���@��@�l�@�K�@�33@�+@�o@���@���@�~�@�M�@�$�@�{@�J@�J@���@��#@�7L@�%@��@�Ĝ@���@�r�@�Z@�b@��@�;d@�o@��@���@���@���@�~�@�v�@�^5@�=q@�-@�{@�x�@��@�j@�1@��;@���@��@��H@��H@��H@�ȴ@�~�@�5?@��7@��@��@��/@�Ĝ@��9@���@��D@�bN@�A�@���@��w@�o@���@��h@��7@��@���@��@�A�@��@��w@��@��@�;d@�o@��H@���@�=q@�@�`B@���@��u@�z�@�r�@�bN@�A�@�w@+@~�@~�+@~�+@~v�@~V@~@}@}�h@}`B@}V@|z�@{ƨ@{��@{t�@{"�@{@z�@z�!@z�\@y��@x��@x�`@x�`@x�`@x�`@x�`@x�`@x��@x�9@w��@v�y@vV@u��@u�h@u�@u�@up�@u/@tI�@t(�@t�@t�@t�@t�@t(�@t(�@t(�@s��@sdZ@sdZ@sS�@sC�@s"�@r�\@r-@rJ@q7L@p�9@pQ�@o�;@o;d@n��@nE�@m�@m�T@m��@mV@l�@l�@l�@k�F@kt�@kdZ@kC�@j��@jn�@j=q@i��@i�@h�`@h�9@h�u@hbN@g��@f�y@f�@f��@f$�@d��@dj@c�
@c��@ct�@cC�@b^5@aX@a�@`��@`Ĝ@`��@`A�@`b@_�w@_
=@^��@^ff@^5?@^@]��@]��@]�@]O�@]/@]V@\�D@\Z@[��@[ƨ@[dZ@Z�@Z��@Z�\@ZM�@Z-@Y�@Y&�@X��@X�@XbN@X1'@W�@WK�@W�@V��@V$�@U�-@U�@T��@Tz�@TI�@T1@S��@SC�@R�H@R��@R^5@Q�#@Qx�@QG�@Q�@P�`@P�@P1'@O�@O\)@N�R@NV@N@M��@MO�@L��@LI�@L1@K��@K�m@K�F@KC�@J��@J�\@J�\@J~�@J^5@J=q@J-@I��@I7L@H��@H1'@G\)@G
=@F��@F�+@Fff@E�@E��@E��@E?}@D��@D��@D��@D9X@C�
@C�
@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@CS�@B�!@B=q@BJ@Ahs@@r�@@b@?�w@?\)@?
=@>�y@>�R@>�+@=�T@=��@=�@=`B@=O�@=�@<��@<9X@<�@;��@;t�@:��@:J@9�7@9&�@8��@8Ĝ@8bN@8 �@7�@7�w@7��@7�@6�+@6E�@6{@5�@5��@5��@5�@5O�@4�@4I�@3�F@3t�@333@2��@2~�@2^5@2M�@2�@1��@1��@17L@0�`@0�u@0b@0  @/�P@/�@.�y@.��@.�+@.v�@.E�@.5?@-�T@-p�@-�@,��@,Z@,9X@+�
@+��@+�@+�@+t�@+t�@+dZ@+33@+o@*�\@)��@)�7@)�@(��@(Ĝ@(�9@(��@( �@'��@'�@'\)@&�@&�R@&��@&v�@&V@&5?@&{@&@%�@%��@%�-@%��@%`B@%/@$�/@$9X@$1@#ƨ@#��@#dZ@#33@#o@#o@#o@"��@"~�@"M�@!�@!X@ ��@ ��@ ��@ �@ bN@   @�;@�w@��@|�@|�@|�@K�@
=@�@v�@��@`B@�@��@�/@�@j@I�@(�@�@1@1@��@ƨ@��@��@�@S�@"�@�H@��@^5@�@�^@��@��@�7@X@��@bN@ �@�;@�@K�@
=@
=@
=@
=@
=@ȴ@v�@V@5?@�@@�-@�-@��@��@�h@�@O�@/@�@z�@I�@9X@�@�
@�@C�@��@M�@�@J@��@��@�#@��@��@��@��@��@x�@X@hs@hs@hs@G�@�@��@��@��@��@�u@r�@ �@��@�@|�@\)@�@
=@��@�y@�+@v�@ff@{@�h@`B@�@�@�j@I�@��@�m@�F@S�@33@"�@
�@
��@
��@
��@
n�@
=q@
-@	��@	�#@	�^@	��@	x�@	&�@	%@��@�u@r�@1'@b@�;@�@�P@K�@;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AՋDAՍPAՏ\AՑhAՏ\AՍPAՋDAՅA�z�A�\)A��AԅA�bA�C�A���Aѩ�A�r�A��A��A���AГuA�bNA��A�5?A�ƨA΍PA�t�A�E�A��yAͼjA�ffA̸RA��Aə�Aȥ�A�t�AƍPA���A�7LA�bNA��A�=qA�\)A���A��^A��A��TA�;dA�7LA���A��A�$�A�A�7LA��A�\)A��A���A���A�5?A�"�A��A�5?A�"�A�=qA�"�A�E�A�+A�x�A��A�dZA�|�A�-A�hsA�dZA��-A�I�A��A��A�|�A���A���A��A�5?A��FA�;dA���A�;dA�%A�/A�&�A��7A���A��A;dA~Az��Av�Ar{Aq�AqS�Ap�Am��Ae&�A`�RA]p�A[�-AY��AX �AV9XAS�;AQ�AP�+AN��AL�uAJ�!AI��AIt�AIG�AH��AHbNAF��AE�AD��ADn�AC�
AB^5A?�^A=;dA;��A;&�A8I�A5�A3��A1x�A/dZA-�A,M�A+C�A)�
A)�A(��A(5?A'A't�A'33A&��A&�A$�HA#��A#l�A#G�A#+A#
=A"�jA!�mA ��A Q�A $�A�PA(�AoA��A��A�A�jA\)A�A�RA��A��A�wA��A1A  A��A+AbA��A�A�A&�A	�A	7LA~�At�A�RAƨA��AJA��AA�\A��A��A ��@��F@��+@���@�
=@���@�=q@�x�@���@��@�@�P@�C�@�"�@�ȴ@�V@�A�@�  @�|�@�K�@�@�@��@땁@�V@�  @��@�\@��#@��@�9X@�P@��@⟾@�ff@���@���@�ƨ@ߥ�@ߥ�@�33@�=q@�O�@�Ĝ@۾w@�|�@�"�@�v�@٩�@�7L@�%@���@��@ؓu@ו�@�ȴ@��T@ԓu@�@�ff@�=q@��@�`B@ϕ�@��y@�n�@�J@��@�hs@��/@�Z@˝�@�o@ʟ�@�V@��T@�hs@�j@��@�@��T@Ł@�/@���@ŉ7@�G�@��
@§�@�+@�J@��^@���@� �@���@�S�@�K�@�
=@���@���@��@�7L@�p�@���@�hs@�p�@��@���@��D@�j@�bN@�b@�dZ@��+@��@��-@���@�9X@�9X@���@�ȴ@�$�@�-@�-@�$�@��@�{@�{@���@��#@���@���@��h@��@�hs@��@���@���@���@��;@�33@���@�^5@�@��@��-@��/@��j@���@���@���@���@���@�j@�9X@�K�@���@�v�@�=q@�M�@��^@�?}@�7L@�/@��@���@�%@���@���@�9X@�(�@���@��;@��F@�@�7L@�%@��`@��9@��D@�bN@� �@�1@��m@���@���@��R@��@�x�@�Ĝ@�z�@�I�@��@���@�;d@�@��R@�ff@�@��@�Q�@�Q�@�b@��@�l�@��H@���@�E�@�-@���@���@�O�@��`@��@��@���@��@�V@��^@��@�?}@�%@��/@��j@���@��@�Z@�9X@�b@�1@���@�C�@��@���@�^5@���@�x�@�`B@��@�I�@��;@���@��@�l�@�K�@�33@�+@�o@���@���@�~�@�M�@�$�@�{@�J@�J@���@��#@�7L@�%@��@�Ĝ@���@�r�@�Z@�b@��@�;d@�o@��@���@���@���@�~�@�v�@�^5@�=q@�-@�{@�x�@��@�j@�1@��;@���@��@��H@��H@��H@�ȴ@�~�@�5?@��7@��@��@��/@�Ĝ@��9@���@��D@�bN@�A�@���@��w@�o@���@��h@��7@��@���@��@�A�@��@��w@��@��@�;d@�o@��H@���@�=q@�@�`B@���@��u@�z�@�r�@�bN@�A�@�w@+@~�@~�+@~�+@~v�@~V@~@}@}�h@}`B@}V@|z�@{ƨ@{��@{t�@{"�@{@z�@z�!@z�\@y��@x��@x�`@x�`@x�`@x�`@x�`@x�`@x��@x�9@w��@v�y@vV@u��@u�h@u�@u�@up�@u/@tI�@t(�@t�@t�@t�@t�@t(�@t(�@t(�@s��@sdZ@sdZ@sS�@sC�@s"�@r�\@r-@rJ@q7L@p�9@pQ�@o�;@o;d@n��@nE�@m�@m�T@m��@mV@l�@l�@l�@k�F@kt�@kdZ@kC�@j��@jn�@j=q@i��@i�@h�`@h�9@h�u@hbN@g��@f�y@f�@f��@f$�@d��@dj@c�
@c��@ct�@cC�@b^5@aX@a�@`��@`Ĝ@`��@`A�@`b@_�w@_
=@^��@^ff@^5?@^@]��@]��@]�@]O�@]/@]V@\�D@\Z@[��@[ƨ@[dZ@Z�@Z��@Z�\@ZM�@Z-@Y�@Y&�@X��@X�@XbN@X1'@W�@WK�@W�@V��@V$�@U�-@U�@T��@Tz�@TI�@T1@S��@SC�@R�H@R��@R^5@Q�#@Qx�@QG�@Q�@P�`@P�@P1'@O�@O\)@N�R@NV@N@M��@MO�@L��@LI�@L1@K��@K�m@K�F@KC�@J��@J�\@J�\@J~�@J^5@J=q@J-@I��@I7L@H��@H1'@G\)@G
=@F��@F�+@Fff@E�@E��@E��@E?}@D��@D��@D��@D9X@C�
@C�
@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@Cƨ@CS�@B�!@B=q@BJ@Ahs@@r�@@b@?�w@?\)@?
=@>�y@>�R@>�+@=�T@=��@=�@=`B@=O�@=�@<��@<9X@<�@;��@;t�@:��@:J@9�7@9&�@8��@8Ĝ@8bN@8 �@7�@7�w@7��@7�@6�+@6E�@6{@5�@5��@5��@5�@5O�@4�@4I�@3�F@3t�@333@2��@2~�@2^5@2M�@2�@1��@1��@17L@0�`@0�u@0b@0  @/�P@/�@.�y@.��@.�+@.v�@.E�@.5?@-�T@-p�@-�@,��@,Z@,9X@+�
@+��@+�@+�@+t�@+t�@+dZ@+33@+o@*�\@)��@)�7@)�@(��@(Ĝ@(�9@(��@( �@'��@'�@'\)@&�@&�R@&��@&v�@&V@&5?@&{@&@%�@%��@%�-@%��@%`B@%/@$�/@$9X@$1@#ƨ@#��@#dZ@#33@#o@#o@#o@"��@"~�@"M�@!�@!X@ ��@ ��@ ��@ �@ bN@   @�;@�w@��@|�@|�@|�@K�@
=@�@v�@��@`B@�@��@�/@�@j@I�@(�@�@1@1@��@ƨ@��@��@�@S�@"�@�H@��@^5@�@�^@��@��@�7@X@��@bN@ �@�;@�@K�@
=@
=@
=@
=@
=@ȴ@v�@V@5?@�@@�-@�-@��@��@�h@�@O�@/@�@z�@I�@9X@�@�
@�@C�@��@M�@�@J@��@��@�#@��@��@��@��@��@x�@X@hs@hs@hs@G�@�@��@��@��@��@�u@r�@ �@��@�@|�@\)@�@
=@��@�y@�+@v�@ff@{@�h@`B@�@�@�j@I�@��@�m@�F@S�@33@"�@
�@
��@
��@
��@
n�@
=q@
-@	��@	�#@	�^@	��@	x�@	&�@	%@��@�u@r�@1'@b@�;@�@�P@K�@;d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	u�B	u�B	v�B	v�B	v�B	v�B	v�B	x�B	z�B	�B	�VB	�!B
VB
G�B
iyB
� B
��B
�wB
��B
�/B
�B
��BJB�B%�B)�B,B.B,B(�B-B2-BA�BJ�BO�BYBaHBk�Bs�B}�B�1B��B��B�-BĜBŢB��B�fB�BB
=B\B{B�B�B�B!�B#�B%�B$�B$�B"�B �B!�B�B{BPB1BB��B��B�B�BB��BB��B�\B�7B~�BgmB?}BbB
��B
��B
�B
�B
�jB
�DB
A�B
33B
&�B
 �B
�B
�B
�B
VB
  B	�mB	��B	��B	ǮB	B	�-B	{�B	cTB	R�B	I�B	C�B	?}B	8RB	49B	(�B	%�B	�B	�B	DB	+B	B	B	B	B��B��B��B��B��B�B�B�mB�NB�;B�HB�)B�B�B�
B��B�
B�
B�B�/B�BB�TB�fB�fB�fB�sB�mB�B��B��B	B	
=B	uB	�B	�B	�B	{B	uB	\B��B��B�B�B�B�B�B�B��B	VB	uB	�B	)�B	-B	0!B	5?B	6FB	7LB	9XB	@�B	?}B	;dB	6FB	1'B	0!B	-B	'�B	#�B	!�B	"�B	"�B	&�B	#�B	 �B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	%�B	)�B	)�B	&�B	%�B	1'B	0!B	2-B	7LB	6FB	5?B	49B	49B	5?B	6FB	5?B	5?B	5?B	6FB	9XB	<jB	@�B	E�B	J�B	R�B	[#B	\)B	]/B	_;B	`BB	bNB	gmB	hsB	jB	m�B	k�B	jB	iyB	iyB	jB	m�B	n�B	n�B	o�B	x�B	z�B	z�B	z�B	y�B	{�B	� B	�B	�B	�B	�1B	�7B	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	�-B	�'B	�LB	�^B	�^B	�}B	ĜB	ƨB	ƨB	ŢB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�mB	�fB	�`B	�mB	�sB	�sB	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
%B
1B
1B
	7B
	7B

=B

=B
DB
JB
PB
PB
PB
PB
VB
PB
VB
VB
VB
VB
VB
\B
\B
\B
bB
bB
bB
hB
uB
oB
oB
uB
uB
oB
oB
oB
uB
uB
uB
uB
uB
uB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
"�B
#�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
#�B
$�B
$�B
$�B
%�B
%�B
&�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
,B
,B
,B
+B
-B
-B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
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
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
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
9XB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
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
>wB
?}B
>wB
>wB
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
A�B
A�B
A�B
A�B
B�B
C�B
B�B
C�B
B�B
C�B
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
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
K�B
K�B
L�B
L�B
M�B
L�B
M�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
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
XB
XB
XB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
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
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
aHB
bNB
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
cTB
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
ffB
ffB
gmB
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
jB
jB
jB
jB
jB
jB
jB
jB
k�B
l�B
l�B
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
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
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
u�B
u�B
v�B
v�B
v�B
v�B
w�B
v�B
w�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
y�B
y�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	u�B	u�B	v�B	v�B	v�B	v�B	v�B	y B	{EB	��B	�B	�eB
�B
I8B
jB
��B
��B
�:B
�yB
��B
�kB
�B�B!B&�B*uB,�B/�B,�B*zB/�B8�BFdBM�BTB\GBd Bm�Bv�B�B�xB��B�B�B�{B�~B�wB��B�	B�B�B�B"B�B�B �B%B(lB0�B)iB'�B$HB%�B*PB%�BvBFB�B�B�B��B�jB��B�CB΀B��B�$B��B��Bw�BNbB�B
�EB
��B
�8B
�:B
�[B
�SB
D�B
7�B
)�B
)[B
�B
B
�B
�B
YB	�AB	�]B	�UB	�B	�-B	�LB	�=B	kLB	WLB	NB	H2B	DJB	>(B	8�B	,B	)�B	�B	�B	.B	�B	�B	5B	/B	�B	 �B��B�dB�^B�qB�B��B�B�B�B�B�B߱B��B�cB�SB��B�B�BޞB�yB�B�NB�/B�@B��B�B�B�B�tB	�B	
�B	cB	�B	 �B	�B	�B	�B	B�'B��B�?B��B��B�zB�B�B��B	 B	�B	!kB	+�B	-MB	1CB	6�B	9LB	:kB	:�B	BEB	A�B	>�B	8�B	3MB	39B	/<B	*�B	'PB	#pB	#�B	#DB	*yB	%�B	!�B	#B	"tB	B	hB	�B	IB	DB	�B	�B	sB	�B	B	B	�B	<B	 B	�B	B	mB	B	2B	 �B	$�B	&�B	'�B	-,B	+�B	'�B	'B	2�B	1B	3B	83B	6�B	5�B	4�B	6B	6QB	6�B	5[B	5�B	6B	7|B	:)B	=�B	@�B	F5B	K�B	TB	[�B	\�B	]_B	__B	`�B	c�B	h�B	i�B	l@B	o�B	laB	j�B	i�B	jOB	l�B	n�B	oLB	o(B	o�B	y�B	{�B	{�B	{�B	z�B	|�B	�tB	��B	��B	�_B	��B	�tB	��B	�B	��B	��B	�B	�NB	��B	�uB	�B	��B	��B	��B	�dB	�$B	��B	��B	�B	�:B	�B	ƫB	ǙB	ȀB	ʦB	�/B	��B	ϵB	�4B	�`B	�1B	�B	фB	�B	�JB	��B	�dB	�LB	��B	�B	�B	�hB	�B	�B	�.B	�RB	�`B	�]B	�YB	��B	�B	�tB	�B	�B	�B	�B	��B	�B	�B	�	B	�B	�B	�B	�)B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�B	�aB	�B	��B	�B	�fB	�IB	��B	��B	��B	�	B	��B	��B	�B	��B	�B	�.B	�B	�(B	��B	�8B	�0B	�B	�.B	�B	�%B	�BB	�B	�B	�B	�EB	�.B	��B	��B	�B	�tB	�OB	�UB	��B	��B	�eB	�~B	��B
 B
 B
IB
-B
�B
�B
�B
B
�B
�B
iB
�B
�B
�B


B
	�B

B
B
�B
nB
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
`B
B
�B
1B
�B
�B
�B
B
�B
DB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"B
�B
(B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
\B
B
EB
�B
'B
�B
�B
�B
CB
PB
�B
 �B
 $B
�B
 B
 �B
 �B
�B
!B
!
B
!:B
!+B
!�B
#kB
$8B
#�B
#�B
#�B
$�B
#BB
#^B
$/B
$
B
$'B
$PB
%2B
$8B
%WB
%�B
%�B
&�B
&�B
'QB
&B
&B
&B
'-B
'nB
'YB
(<B
(:B
(B
(B
( B
)EB
)7B
)0B
)0B
)IB
)uB
)�B
*5B
*-B
*KB
+5B
+)B
+KB
+9B
+�B
,�B
,.B
,$B
,%B
,#B
,$B
,%B
,1B
,@B
+�B
-�B
-�B
.�B
.]B
.<B
.2B
.>B
.eB
.�B
/LB
/BB
/7B
/4B
/4B
/-B
/7B
/8B
/�B
0iB
0=B
0CB
0IB
0[B
0�B
1�B
1^B
1�B
1�B
1�B
2�B
2�B
3�B
3�B
3�B
3YB
3B
3�B
4jB
4VB
4�B
4�B
5�B
5gB
5qB
5�B
5�B
6�B
6�B
6�B
7�B
7�B
7|B
7�B
7�B
8�B
9�B
9�B
9�B
:1B
:�B
;�B
;�B
;�B
;�B
<B
=)B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
>B
=�B
>�B
>�B
>�B
>�B
?�B
>�B
>�B
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
A�B
A�B
A�B
B7B
CB
C�B
B�B
C�B
B�B
C�B
C�B
C�B
E	B
D�B
EB
E�B
E�B
E�B
E�B
F	B
F�B
GB
F�B
HB
HB
HB
H�B
H�B
H�B
IB
IB
H�B
I8B
JIB
JB
KB
KB
K;B
LgB
LB
LB
L�B
K�B
L
B
M?B
M\B
M�B
L�B
M�B
MB
NB
M�B
N;B
N\B
N6B
O{B
O�B
P@B
PAB
PB
Q!B
QXB
Q;B
QB
PCB
Q2B
QB
QMB
QNB
QKB
RB
QB
R	B
RB
RB
RB
R	B
RB
R
B
RB
RB
QTB
RzB
SbB
S6B
S�B
T�B
T]B
TJB
TXB
URB
U2B
U;B
VBB
V�B
VKB
W>B
W;B
W3B
WHB
WB
XoB
XCB
XDB
Y�B
Y�B
Z�B
Z�B
ZxB
[]B
[^B
[�B
[lB
[`B
\hB
\_B
\�B
\�B
\qB
]jB
]bB
]`B
]jB
]aB
^sB
^�B
^�B
^�B
_�B
_�B
_�B
`�B
`sB
`gB
`�B
a�B
a�B
a�B
b�B
a�B
b�B
byB
b�B
b�B
b�B
b�B
b|B
c|B
c�B
c{B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
e�B
e�B
e|B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
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
j�B
j�B
k�B
l�B
mB
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
n�B
oB
pB
o�B
o�B
o�B
o�B
pB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
rkB
q�B
sB
r�B
r�B
r�B
sB
s�B
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
uB
uB
uB
uB
v?B
vB
u�B
u�B
u�B
vB
v4B
vXB
vB
wB
wB
w3B
wB
w�B
v�B
w�B
v�B
x!B
x,B
xB
x	B
x#B
xB
x�B
x�B
x�B
x�B
x�B
yB
yB
zB
z0B
zSB
zB
{B
zB
z,B
{:B
{6B
{]B
|jB
|+B
|B
|B
}B
}"B
}B
}B
}'B
}B
}B
}"B
~%B
~B
~B
~B
~*B
~8B
~'B
~B
~B
~B
~^B
-B
XB
TB
3B
<B
.B
AB
$B
 B
%B
^B
�(B
�&B
�NB
�iB
�?B
�IB
�<B
�?B
�eB
�WB
�1B
�IB
�bB
�AB
�6B
�HB
�?B
�7B
�@B
�JB
�RB
�AB
�SB
�FB
�FB
�KB
�IB
�dB
�MB
�WB
�`B
�QB
�`B
�UB
�]B
�^B
�RB
�cB
�CB
�I11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<?ϝ<#�
<#�
<#�
<#�
<#�
<)D�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<)��<O�O<#�
<#�
<#�
<#�
<���<{��<#�
<#�
<#�
<#�
<#�
<m�%<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<G0<1O<#�
<#�
<#�
<#�
<�-<8��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.21 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  202007211526432020072115264320200721152643  AO  ARCAADJP                                                                    20190511110029    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20190511110029  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20190511110029  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20200721152643  QC  PRES            @�ffD�)�G�O�                PM  ARSQCTM V1.1                                                                20200721152643  QC  PSAL            @�ffD�)�G�O�                PM  ARSQCOWGV1.1CTD_2018v2 + Argo_2018v01                                       20200901151524  IP                  G�O�G�O�G�O�                