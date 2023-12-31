CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-08-07T09:00:35Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `p   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɸ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ͠   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20210807090035  20210807090035  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               rA   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @ى�V��1   @ى���UF@;7
=p���c� ě��1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         rA   A   F   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@�G�@�G�A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/�\B7(�B>BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{B�aHB�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�)Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�Dur�Du�Dvr�Dv�Dwr�Dw�Dxr�Dx�Dyr�Dy�Dzr�Dz�D{r�D{�D|r�D|�D}r�D}�D~r�D~�Dr�D�D�9HD�yHD��HD��HD�9HD�|{D��HD��HD�6D�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�vD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD¹HD��HD�9HD�yHDùHD��HD�9HD�yHDĹHD��HD�9HD�yHDŹHD��HD�9HD�yHDƹHD��HD�9HD�yHDǹHD��HD�9HD�yHDȹHD��HD�9HD�yHDɹHD��HD�9HD�yHDʹHD��HD�9HD�yHD˹HD��HD�9HD�yHD̹HD��HD�9HD�yHD͹HD��HD�9HD�yHDιHD��HD�9HD�yHDϹHD��HD�9HD�yHDйHD��HD�9HD�yHDѹHD��HD�9HD�yHDҹHD��HD�9HD�yHDӹHD��HD�9HD�yHDԹHD��HD�9HD�yHDչHD��HD�9HD�yHDֹHD��HD�9HD�yHD׹HD��HD�9HD�yHDعHD��HD�9HD�yHDٹHD��HD�9HD�yHDڹHD��HD�9HD�yHD۹HD��HD�9HD�yHDܹHD��HD�9HD�yHDݹHD��HD�9HD�yHD޹HD��HD�9HD�yHD߹HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD��HD��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AΏ\AΑhAΓuAΓuAΕ�AΗ�AΙ�AΙ�AΗ�AΙ�AΛ�AΝ�AΗ�AΗ�AΕ�AΓuAΙ�AΛ�AΑhA�z�A�+Aɇ+A���A���A�K�A�ȴA��TA��`A�&�A��/A��A�S�A��A�?}A���A�dZA�\)A�  A�{A�M�A�1'A��;A���A�oA�ffA�A�bNA��uA���A��PA���A�9XA�jA� �A��A�l�A���A�{A�O�A��DA�G�A�$�A�JA��FA�9XA��PA�C�A�A��hA��A���A�;dA�x�A�7LA��A�A�G�A��A�;dA�(�A���A�&�A�7LA�l�A�1A���A��A��;A�7LA��`A��+A�p�A�oA���A�^5A� �A��uA�~�A��A�{A�n�A��A~jA}�^A{�^Az�HAzv�Azn�Ay�Ax�jAt�yAs�^Ar�Ap�Ao+An��Am�mAlM�AlJAk�7AjZAi
=Ag��Af5?Aex�AdĜAc�Ab�/A`�uA_�A]�wA[hsAY/AW��AU�^AS��AR��ARz�ARAP�AO�hANVAL(�AJ�DAI��AI/AH-AGx�AF�uAE��AE�ADI�ACAB�HAB�!A@�HA?;dA<�A;�mA;/A:^5A8��A7��A6�A65?A5hsA4�`A45?A3hsA1A0�A/��A.^5A-��A-7LA,��A,z�A,(�A+�#A+
=A)�
A(-A'�TA'��A'�^A&�`A%�A%O�A$��A#�#A#C�A"�RA"jA!�
A!?}A =qA�TA��AQ�A�wA��A��A�/A��AbNA;dAZA7LA�AȴA��A�A  At�A��A��A��A1'A��AdZA�A�^A��AM�A��A
1A	`BA��AQ�A��A��A�A�A1A��A��A�!AZA�
AO�A bN@��@��7@�j@�o@�E�@�G�@��@�@��/@��@�@�=q@�^@���@���@���@�ff@�@�%@�Z@��@�o@�%@��@�^@�A�@�@�{@�"�@��/@�\)@ٺ^@��@���@���@�ȴ@���@θR@�/@���@��H@ə�@��@�x�@�+@�ȴ@�E�@��/@�|�@���@��@��
@��@�;d@�v�@�@�X@��D@��@�"�@��@��^@��@�  @�\)@�v�@�O�@�Q�@�+@���@���@��@�j@�(�@� �@��F@�K�@�@�n�@�@��@�Ĝ@��D@�j@�1'@��
@�o@���@�M�@�@�@�O�@��@��j@�b@�S�@�
=@�ȴ@�5?@�@�`B@�1@��F@���@�l�@��@��#@���@��D@�r�@� �@��H@�n�@�J@�@�X@���@�A�@��
@�l�@��+@�{@��@�?}@�Z@��@�l�@�dZ@�;d@��H@���@��!@��+@�ff@�V@��+@���@�~�@�ff@���@�`B@���@�r�@� �@��
@�ƨ@���@�@���@�^5@�$�@��7@��@���@��j@��j@��9@���@��u@��D@��D@��D@�z�@�r�@�j@�Z@�9X@���@�K�@��R@�ff@�-@�@�7L@��@��@���@���@�S�@�;d@��@�o@�
=@��@�ȴ@�ff@��#@�`B@��@��@���@���@���@�j@�  @���@���@�t�@�\)@�+@��H@��R@�v�@�@���@�hs@�X@��@��`@���@��9@�I�@�b@�1@�1@�@�w@~��@~��@~v�@~ff@~ff@~V@~{@}�T@}��@}`B@}�@|�@|�@{��@z��@z^5@y�@y��@y�#@y7L@y�@x�`@xĜ@xbN@wl�@v��@v��@v�@v�@vv�@vv�@v{@v@v@v@v@v@v@v{@vȴ@v��@vE�@u@u`B@u/@t��@t�@t��@s��@s�
@s�
@sƨ@s�F@s"�@r�@r�!@rn�@rn�@r�@q�#@q��@qhs@p �@nv�@nE�@n�+@nff@m�@m@m�h@mO�@m�@l�/@lI�@l9X@l�@l�@l(�@l(�@k��@k�F@k33@j^5@ihs@h�`@g�w@f�@e�@e��@e�@eV@d��@d�/@dz�@d9X@d�@d�@cƨ@c��@c�@cdZ@cS�@c33@b��@b��@b�!@b�\@bM�@a��@ahs@a&�@`��@`�u@` �@_�;@_��@_�@_�P@_+@^�y@^$�@]`B@\�@\j@\9X@[��@[33@Z�H@Z��@Zn�@Z-@Y��@Y&�@X�`@X�u@X1'@WK�@V�R@V�y@V�@V�R@V�+@V@U@U�@T��@T�@Sƨ@S��@St�@St�@SdZ@SC�@R�!@Q��@Q�7@Q�7@Qx�@Q&�@Q%@P��@Pr�@PA�@O�@O�P@O+@Nȴ@NV@N$�@M�-@M`B@L��@L�D@Lj@LI�@L(�@Kƨ@K�@KS�@Ko@J��@J~�@I��@I�7@I7L@I%@HĜ@H  @G��@G��@G|�@Gl�@G
=@F�+@F5?@F@E@E�-@E��@E��@Ep�@E?}@D�@D�j@D��@D��@Dj@D9X@Cƨ@C�F@C�F@Ct�@C@B��@B=q@A�@A��@A%@@�u@@Q�@@ �@?�w@?��@?|�@?;d@?
=@>��@>V@>@=�h@=�h@=/@<��@<�D@<I�@;�F@;t�@;S�@:�H@:��@:^5@:=q@:�@9��@9�7@9&�@9%@8��@8Ĝ@8�u@81'@7�@7;d@7
=@6��@5�T@5�-@5/@4��@4Z@49X@4�@41@3�m@3�F@3S�@3@2�\@2=q@1��@1��@1��@1�7@1X@1G�@1G�@1&�@0��@0�9@0�u@0A�@0  @/�@/�P@/�P@/+@.��@.v�@.5?@-��@-�h@-`B@-/@,�/@,�D@,9X@+�m@+�F@+dZ@+"�@*�@*�H@*�H@*�H@*�H@*��@*�\@*M�@)��@)�@)�@)�#@)��@)��@)�7@)hs@)7L@)�@(�`@(�9@(�u@(Q�@'��@'�w@'�@'�P@'K�@&��@&v�@&5?@&$�@%�T@%��@%�-@%`B@%/@$�@$�j@$��@$��@$�D@$Z@#�
@#��@#S�@#C�@#"�@#@"��@"��@"~�@"�@!��@!�^@!x�@!�@!%@ �9@ bN@ Q�@ A�@   @�;@��@�@;d@;d@;d@+@�@ȴ@V@V@E�@5?@$�@�@�T@�-@�h@O�@O�@/@?}@�/@z�@Z@(�@1@�
@��@S�@o@��@��@n�@-@J@��@��@��@x�@X@&�@��@��@Ĝ@�@bN@A�@�;@�P@\)@K�@K�@K�@�@ȴ@v�@5?@$�@�@�-@p�@O�@?}@?}@�@��@�@��@��@��@��@z�@(�@�@"�@@�@��@~�@^5@=q@�@J@�@��@��@hs@X@�@%@��@Q�@1'@1'@ �@  @�@�;@�w@�@�P@;d@��@�@��@ff@E�@�@@��@�@O�@�@��@��@�/@�/@��@�j@��@z�@I�@�m@�
@�
@ƨ@��@�@dZ@"�@
�H@
��@
��@
�\@
J@	�#@	��@	��@	�7@	7L@��@��@��@�@A�@1'@ �@�@�;@�@l�@\)@\)@K�@;d@
=@��@�@�@ȴ@�R@��@v�@$�@@�@�T@�T@�T@�T@��@?}@V@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AΏ\AΑhAΓuAΓuAΕ�AΗ�AΙ�AΙ�AΗ�AΙ�AΛ�AΝ�AΗ�AΗ�AΕ�AΓuAΙ�AΛ�AΑhA�z�A�+Aɇ+A���A���A�K�A�ȴA��TA��`A�&�A��/A��A�S�A��A�?}A���A�dZA�\)A�  A�{A�M�A�1'A��;A���A�oA�ffA�A�bNA��uA���A��PA���A�9XA�jA� �A��A�l�A���A�{A�O�A��DA�G�A�$�A�JA��FA�9XA��PA�C�A�A��hA��A���A�;dA�x�A�7LA��A�A�G�A��A�;dA�(�A���A�&�A�7LA�l�A�1A���A��A��;A�7LA��`A��+A�p�A�oA���A�^5A� �A��uA�~�A��A�{A�n�A��A~jA}�^A{�^Az�HAzv�Azn�Ay�Ax�jAt�yAs�^Ar�Ap�Ao+An��Am�mAlM�AlJAk�7AjZAi
=Ag��Af5?Aex�AdĜAc�Ab�/A`�uA_�A]�wA[hsAY/AW��AU�^AS��AR��ARz�ARAP�AO�hANVAL(�AJ�DAI��AI/AH-AGx�AF�uAE��AE�ADI�ACAB�HAB�!A@�HA?;dA<�A;�mA;/A:^5A8��A7��A6�A65?A5hsA4�`A45?A3hsA1A0�A/��A.^5A-��A-7LA,��A,z�A,(�A+�#A+
=A)�
A(-A'�TA'��A'�^A&�`A%�A%O�A$��A#�#A#C�A"�RA"jA!�
A!?}A =qA�TA��AQ�A�wA��A��A�/A��AbNA;dAZA7LA�AȴA��A�A  At�A��A��A��A1'A��AdZA�A�^A��AM�A��A
1A	`BA��AQ�A��A��A�A�A1A��A��A�!AZA�
AO�A bN@��@��7@�j@�o@�E�@�G�@��@�@��/@��@�@�=q@�^@���@���@���@�ff@�@�%@�Z@��@�o@�%@��@�^@�A�@�@�{@�"�@��/@�\)@ٺ^@��@���@���@�ȴ@���@θR@�/@���@��H@ə�@��@�x�@�+@�ȴ@�E�@��/@�|�@���@��@��
@��@�;d@�v�@�@�X@��D@��@�"�@��@��^@��@�  @�\)@�v�@�O�@�Q�@�+@���@���@��@�j@�(�@� �@��F@�K�@�@�n�@�@��@�Ĝ@��D@�j@�1'@��
@�o@���@�M�@�@�@�O�@��@��j@�b@�S�@�
=@�ȴ@�5?@�@�`B@�1@��F@���@�l�@��@��#@���@��D@�r�@� �@��H@�n�@�J@�@�X@���@�A�@��
@�l�@��+@�{@��@�?}@�Z@��@�l�@�dZ@�;d@��H@���@��!@��+@�ff@�V@��+@���@�~�@�ff@���@�`B@���@�r�@� �@��
@�ƨ@���@�@���@�^5@�$�@��7@��@���@��j@��j@��9@���@��u@��D@��D@��D@�z�@�r�@�j@�Z@�9X@���@�K�@��R@�ff@�-@�@�7L@��@��@���@���@�S�@�;d@��@�o@�
=@��@�ȴ@�ff@��#@�`B@��@��@���@���@���@�j@�  @���@���@�t�@�\)@�+@��H@��R@�v�@�@���@�hs@�X@��@��`@���@��9@�I�@�b@�1@�1@�@�w@~��@~��@~v�@~ff@~ff@~V@~{@}�T@}��@}`B@}�@|�@|�@{��@z��@z^5@y�@y��@y�#@y7L@y�@x�`@xĜ@xbN@wl�@v��@v��@v�@v�@vv�@vv�@v{@v@v@v@v@v@v@v{@vȴ@v��@vE�@u@u`B@u/@t��@t�@t��@s��@s�
@s�
@sƨ@s�F@s"�@r�@r�!@rn�@rn�@r�@q�#@q��@qhs@p �@nv�@nE�@n�+@nff@m�@m@m�h@mO�@m�@l�/@lI�@l9X@l�@l�@l(�@l(�@k��@k�F@k33@j^5@ihs@h�`@g�w@f�@e�@e��@e�@eV@d��@d�/@dz�@d9X@d�@d�@cƨ@c��@c�@cdZ@cS�@c33@b��@b��@b�!@b�\@bM�@a��@ahs@a&�@`��@`�u@` �@_�;@_��@_�@_�P@_+@^�y@^$�@]`B@\�@\j@\9X@[��@[33@Z�H@Z��@Zn�@Z-@Y��@Y&�@X�`@X�u@X1'@WK�@V�R@V�y@V�@V�R@V�+@V@U@U�@T��@T�@Sƨ@S��@St�@St�@SdZ@SC�@R�!@Q��@Q�7@Q�7@Qx�@Q&�@Q%@P��@Pr�@PA�@O�@O�P@O+@Nȴ@NV@N$�@M�-@M`B@L��@L�D@Lj@LI�@L(�@Kƨ@K�@KS�@Ko@J��@J~�@I��@I�7@I7L@I%@HĜ@H  @G��@G��@G|�@Gl�@G
=@F�+@F5?@F@E@E�-@E��@E��@Ep�@E?}@D�@D�j@D��@D��@Dj@D9X@Cƨ@C�F@C�F@Ct�@C@B��@B=q@A�@A��@A%@@�u@@Q�@@ �@?�w@?��@?|�@?;d@?
=@>��@>V@>@=�h@=�h@=/@<��@<�D@<I�@;�F@;t�@;S�@:�H@:��@:^5@:=q@:�@9��@9�7@9&�@9%@8��@8Ĝ@8�u@81'@7�@7;d@7
=@6��@5�T@5�-@5/@4��@4Z@49X@4�@41@3�m@3�F@3S�@3@2�\@2=q@1��@1��@1��@1�7@1X@1G�@1G�@1&�@0��@0�9@0�u@0A�@0  @/�@/�P@/�P@/+@.��@.v�@.5?@-��@-�h@-`B@-/@,�/@,�D@,9X@+�m@+�F@+dZ@+"�@*�@*�H@*�H@*�H@*�H@*��@*�\@*M�@)��@)�@)�@)�#@)��@)��@)�7@)hs@)7L@)�@(�`@(�9@(�u@(Q�@'��@'�w@'�@'�P@'K�@&��@&v�@&5?@&$�@%�T@%��@%�-@%`B@%/@$�@$�j@$��@$��@$�D@$Z@#�
@#��@#S�@#C�@#"�@#@"��@"��@"~�@"�@!��@!�^@!x�@!�@!%@ �9@ bN@ Q�@ A�@   @�;@��@�@;d@;d@;d@+@�@ȴ@V@V@E�@5?@$�@�@�T@�-@�h@O�@O�@/@?}@�/@z�@Z@(�@1@�
@��@S�@o@��@��@n�@-@J@��@��@��@x�@X@&�@��@��@Ĝ@�@bN@A�@�;@�P@\)@K�@K�@K�@�@ȴ@v�@5?@$�@�@�-@p�@O�@?}@?}@�@��@�@��@��@��@��@z�@(�@�@"�@@�@��@~�@^5@=q@�@J@�@��@��@hs@X@�@%@��@Q�@1'@1'@ �@  @�@�;@�w@�@�P@;d@��@�@��@ff@E�@�@@��@�@O�@�@��@��@�/@�/@��@�j@��@z�@I�@�m@�
@�
@ƨ@��@�@dZ@"�@
�H@
��@
��@
�\@
J@	�#@	��@	��@	�7@	7L@��@��@��@�@A�@1'@ �@�@�;@�@l�@\)@\)@K�@;d@
=@��@�@�@ȴ@�R@��@v�@$�@@�@�T@�T@�T@�T@��@?}@V@��@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�DB�B}�B{�Bz�B{�Bt�Bk�BiyBgmBr�Bp�Bm�Be`B^5BT�BP�BL�BG�B@�B>wB=qB;dB8RB1'B)�B)�B�BuBB�B�mB�#B��BǮBŢB�wB�qB�jB�qB�}B�XB�-B�3B�'B�B��B��B�hB�BjBYBA�B2-B$�B�B\B��B�B�NB��B��BɺBǮB�?B��B�{B}�Bm�BffB_;B[#BVBN�B8RB(�B �BbB%B��B��B�B�fB�TB�NB�5B��B�wB�'B�B��B��B�uB�bB�7B�B�Bx�Bq�BiyB[#BQ�BM�BN�BJ�B?}B7LB'�BbB
��B
��B
�B
�TB
�5B
�/B
�B
�
B
��B
��B
�wB
�B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�DB
�+B
�B
�B
z�B
v�B
t�B
u�B
s�B
q�B
o�B
l�B
hsB
cTB
`BB
\)B
YB
W
B
VB
T�B
R�B
Q�B
O�B
M�B
I�B
G�B
F�B
E�B
D�B
A�B
@�B
=qB
<jB
:^B
9XB
8RB
7LB
5?B
33B
1'B
0!B
.B
,B
+B
'�B
%�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
hB
\B
\B
VB
VB
JB
DB
1B
+B
%B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�mB	�mB	�`B	�fB	�`B	�`B	�`B	�ZB	�ZB	�TB	�TB	�TB	�NB	�HB	�BB	�;B	�5B	�5B	�5B	�5B	�)B	�;B	�;B	�;B	�5B	�/B	�/B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�BB	�ZB	�ZB	�TB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
B
1B
	7B
DB
VB
bB
hB
bB
oB
uB
{B
�B
�B
�B
�B
�B
 �B
!�B
"�B
'�B
)�B
,B
-B
0!B
33B
49B
6FB
9XB
=qB
?}B
@�B
B�B
C�B
E�B
J�B
K�B
K�B
L�B
N�B
T�B
YB
ZB
ZB
[#B
_;B
_;B
aHB
bNB
dZB
gmB
k�B
m�B
o�B
u�B
w�B
y�B
~�B
�B
�7B
�DB
�DB
�JB
�VB
�\B
�hB
�{B
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
�B
�!B
�'B
�3B
�RB
�^B
�jB
�qB
B
ƨB
ȴB
ɺB
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
�B
�)B
�;B
�BB
�`B
�yB
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B  BBDBVB\BhBuB�B�B�B�B�B�B�B�B�B�B�B!�B$�B&�B+B,B.B0!B0!B1'B49B5?B6FB6FB6FB7LB;dB=qB>wB>wB>wB@�BA�BC�BE�BF�BG�BH�BI�BL�BP�BQ�BR�BS�BVBXBYB[#B]/B^5BaHBdZBdZBgmBjBm�Bm�Bo�Bp�Bp�Bp�Bq�Bq�Bq�Bs�B{�B|�B}�B~�B~�B� B�B�B�B�+B�1B�1B�1B�1B�=B�DB�JB�JB�JB�PB�VB�\B�\B�\B�VB�\B�bB�hB�hB�oB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�3B�9B�?B�FB�FB�FB�LB�LB�LB�RB�RB�XB�^B�^B�^B�dB�jB�wB�}B�}B��B��B��BBBBÖBĜBŢBǮBǮBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B�B�B�#B�)B�)B�/B�5B�;B�;B�;B�HB�NB�NB�TB�TB�TB�TB�ZB�mB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  BBBBBBBBBBB%B+B+B1B1B	7B
=B
=BDBDBJBJBPBPBPBPBVBVBVB\BbBbBhBoBoBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B"�B#�B#�B#�B#�B$�B$�B$�B$�B$�B%�B%�B%�B&�B&�B&�B'�B(�B(�B)�B)�B)�B)�B+B+B+B,B,B,B-B.B.B.B.B.B.B.B/B/B0!B0!B0!B0!B0!B0!B0!B1'B1'B1'B1'B1'B1'B2-B2-B2-B33B33B33B49B49B5?B5?B5?B5?B5?B6FB6FB7LB7LB7LB7LB7LB7LB8RB8RB9XB9XB9XB9XB:^B:^B:^B;dB;dB;dB<jB<jB<jB=qB=qB=qB=qB>wB>wB>wB>wB?}B?}B?}B?}B?}B@�B@�B@�BA�BA�BA�BA�BA�BA�BB�BB�BB�BB�BB�BC�BC�BC�BD�BD�BD�BD�BE�BE�BF�BF�BF�BG�BG�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BI�BJ�BK�BK�BK�BK�BK�BK�BL�BL�BL�BM�BM�BM�BN�BN�BN�BN�BN�BO�BO�BO�BO�BO�BO�BO�BO�BQ�BQ�BQ�BQ�BQ�BR�BR�BR�BS�BS�BS�BS�BS�BT�BT�BT�BT�BT�BVBW
BW
BW
BW
BW
BW
BW
BXBXBXBYBYBYBYBZBZBZB[#B[#B[#B[#B\)B\)B\)B\)B\)B\)B\)B\)B]/B]/B]/B]/B^5B^5B^5B^5B_;B_;B_;B_;B_;B`BB`BB`BBaHBaHBaHBbNBbNBbNBbNBcTBcTBcTBcTBcTBdZBdZBdZBdZBdZBdZBe`Be`Be`Be`Be`Be`Be`Be`BffBffBffBffBffBffBffBgmBgmBhsBhsBhsBhsBhs44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�DB�B}�B{�Bz�B{�Bt�Bk�BiyBgmBr�Bp�Bm�Be`B^5BT�BP�BL�BG�B@�B>wB=qB;dB8RB1'B)�B)�B�BuBB�B�mB�#B��BǮBŢB�wB�qB�jB�qB�}B�XB�-B�3B�'B�B��B��B�hB�BjBYBA�B2-B$�B�B\B��B�B�NB��B��BɺBǮB�?B��B�{B}�Bm�BffB_;B[#BVBN�B8RB(�B �BbB%B��B��B�B�fB�TB�NB�5B��B�wB�'B�B��B��B�uB�bB�7B�B�Bx�Bq�BiyB[#BQ�BM�BN�BJ�B?}B7LB'�BbB
��B
��B
�B
�TB
�5B
�/B
�B
�
B
��B
��B
�wB
�B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�DB
�+B
�B
�B
z�B
v�B
t�B
u�B
s�B
q�B
o�B
l�B
hsB
cTB
`BB
\)B
YB
W
B
VB
T�B
R�B
Q�B
O�B
M�B
I�B
G�B
F�B
E�B
D�B
A�B
@�B
=qB
<jB
:^B
9XB
8RB
7LB
5?B
33B
1'B
0!B
.B
,B
+B
'�B
%�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
hB
\B
\B
VB
VB
JB
DB
1B
+B
%B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�mB	�mB	�`B	�fB	�`B	�`B	�`B	�ZB	�ZB	�TB	�TB	�TB	�NB	�HB	�BB	�;B	�5B	�5B	�5B	�5B	�)B	�;B	�;B	�;B	�5B	�/B	�/B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�BB	�ZB	�ZB	�TB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
B
1B
	7B
DB
VB
bB
hB
bB
oB
uB
{B
�B
�B
�B
�B
�B
 �B
!�B
"�B
'�B
)�B
,B
-B
0!B
33B
49B
6FB
9XB
=qB
?}B
@�B
B�B
C�B
E�B
J�B
K�B
K�B
L�B
N�B
T�B
YB
ZB
ZB
[#B
_;B
_;B
aHB
bNB
dZB
gmB
k�B
m�B
o�B
u�B
w�B
y�B
~�B
�B
�7B
�DB
�DB
�JB
�VB
�\B
�hB
�{B
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
�B
�!B
�'B
�3B
�RB
�^B
�jB
�qB
B
ƨB
ȴB
ɺB
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
�B
�)B
�;B
�BB
�`B
�yB
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B  BBDBVB\BhBuB�B�B�B�B�B�B�B�B�B�B�B!�B$�B&�B+B,B.B0!B0!B1'B49B5?B6FB6FB6FB7LB;dB=qB>wB>wB>wB@�BA�BC�BE�BF�BG�BH�BI�BL�BP�BQ�BR�BS�BVBXBYB[#B]/B^5BaHBdZBdZBgmBjBm�Bm�Bo�Bp�Bp�Bp�Bq�Bq�Bq�Bs�B{�B|�B}�B~�B~�B� B�B�B�B�+B�1B�1B�1B�1B�=B�DB�JB�JB�JB�PB�VB�\B�\B�\B�VB�\B�bB�hB�hB�oB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�3B�9B�?B�FB�FB�FB�LB�LB�LB�RB�RB�XB�^B�^B�^B�dB�jB�wB�}B�}B��B��B��BBBBÖBĜBŢBǮBǮBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B�B�B�#B�)B�)B�/B�5B�;B�;B�;B�HB�NB�NB�TB�TB�TB�TB�ZB�mB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  BBBBBBBBBBB%B+B+B1B1B	7B
=B
=BDBDBJBJBPBPBPBPBVBVBVB\BbBbBhBoBoBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B"�B#�B#�B#�B#�B$�B$�B$�B$�B$�B%�B%�B%�B&�B&�B&�B'�B(�B(�B)�B)�B)�B)�B+B+B+B,B,B,B-B.B.B.B.B.B.B.B/B/B0!B0!B0!B0!B0!B0!B0!B1'B1'B1'B1'B1'B1'B2-B2-B2-B33B33B33B49B49B5?B5?B5?B5?B5?B6FB6FB7LB7LB7LB7LB7LB7LB8RB8RB9XB9XB9XB9XB:^B:^B:^B;dB;dB;dB<jB<jB<jB=qB=qB=qB=qB>wB>wB>wB>wB?}B?}B?}B?}B?}B@�B@�B@�BA�BA�BA�BA�BA�BA�BB�BB�BB�BB�BB�BC�BC�BC�BD�BD�BD�BD�BE�BE�BF�BF�BF�BG�BG�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BI�BJ�BK�BK�BK�BK�BK�BK�BL�BL�BL�BM�BM�BM�BN�BN�BN�BN�BN�BO�BO�BO�BO�BO�BO�BO�BO�BQ�BQ�BQ�BQ�BQ�BR�BR�BR�BS�BS�BS�BS�BS�BT�BT�BT�BT�BT�BVBW
BW
BW
BW
BW
BW
BW
BXBXBXBYBYBYBYBZBZBZB[#B[#B[#B[#B\)B\)B\)B\)B\)B\)B\)B\)B]/B]/B]/B]/B^5B^5B^5B^5B_;B_;B_;B_;B_;B`BB`BB`BBaHBaHBaHBbNBbNBbNBbNBcTBcTBcTBcTBcTBdZBdZBdZBdZBdZBdZBe`Be`Be`Be`Be`Be`Be`Be`BffBffBffBffBffBffBffBgmBgmBhsBhsBhsBhsBhs44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.21 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210807090035                              AO  ARCAADJP                                                                    20210807090035    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210807090035  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210807090035  QCF$                G�O�G�O�G�O�8000            