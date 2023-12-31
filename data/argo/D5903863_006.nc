CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-07-21T23:38:45Z creation; 2014-07-21T23:38:45Z updated; 2015-09-28T12:13:14Z converted from 3.0   
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7`   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8$   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8D   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8d   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           8h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8p   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8t   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8|   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Hp   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ^�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  mh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ޔ   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ޤ   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ި   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ޸   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ޼   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20140721233845  20170523133323  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  4298_0127_006                   2C  D   NAVIS_A                         0127                            120111                          863 @�.����1   @�.�r��
@5��t�j�d������1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      A   A   A   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D칚1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @x��@�{@�G�A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�BB(�B(�B(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{��C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�Dur�Du�Dvr�Dv�Dwr�Dw�Dxr�Dx�Dyr�Dy�Dzr�Dz�D{r�D{�D|r�D|�D}r�D}�D~r�D~�Dr�D�D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD¹HD��HD�9HD�yHDùHD��HD�9HD�yHDĹHD��HD�9HD�yHDŹHD��HD�9HD�yHDƹHD��HD�9HD�yHDǹHD��HD�9HD�yHDȹHD��HD�9HD�yHDɹHD��HD�9HD�yHDʹHD��HD�9HD�yHD˹HD��HD�9HD�yHD̹HD��HD�9HD�yHD͹HD��HD�9HD�yHDιHD��HD�9HD�yHDϹHD��HD�9HD�yHDйHD��HD�9HD�yHDѹHD��HD�9HD�yHDҹHD��HD�9HD�yHDӹHD��HD�9HD�yHDԹHD��HD�9HD�yHDչHD��HD�9HD�yHDֹHD��HD�9HD�yHD׹HD��HD�9HD�yHDعHD��HD�9HD�yHDٹHD��HD�9HD�yHDڹHD��HD�9HD�yHD۹HD��HD�9HD�yHDܹHD��HD�9HD�yHDݹHD��HD�9HD�yHD޹HD��HD�9HD�yHD߹HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�<{D�|{D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�r�A�p�A�p�A�p�A�r�A�t�A�t�A�v�A��A��A��A��+A��+A��+A��DA��DA��DA��PA��\A��\A��PA��\A��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��!A��!A��!A��-A��9A��FA��9A��!A���A���A���A��A�VA��yA�;dA��+A���A���A���A��A��A��A��A�M�A�x�A���A���A��A� �A�5?A��A�1A���A��HA�JA���A��A�ƨA�9XA� �A�r�A��A�ƨA���A�1'A���A�n�A�$�A��`A��wA���A���A�1'A��A�A�K�A�/A���A�hsA��A�r�A�ĜA��`A�v�A�jA�1'A�VA���A���A��TA�Q�A�5?A��A���A~��A~�DA~bA|�A{��Az�Ay�Av�\Au%As�FAr��Ar�Aq�^Ao�TAl5?Aj1'Ai��Ah��Ag/Ae�-AdVAa|�A`bA]�7A[�hA[7LAZbNAX~�AU�mAUVAS��ARjAP1'AN�AL=qAJZAI�#AH  AG��AG�AEdZAC�
AAK�A@Q�A=�A<�A<E�A;��A;K�A;
=A:�RA:^5A9��A8ĜA8�A6��A4ȴA3?}A1�A0�A.E�A-��A,��A+�PA)��A(�uA'x�A&�A%7LA%�A$=qA"��A ĜAA��A��AbA��Ap�A=qA�TA��A^5A�AoAE�A��AbNA1A��A"�A��A+A��An�A{A�RA�mA?}A
bA=qAz�A�wA5?A�;A\)A�!AVAbA&�A   @�V@�z�@�9X@�l�@�@�?}@�j@��P@�v�@�@���@���@�r�@��H@�@�=q@�V@�w@���@�+@�+@��@�b@�{@��;@�$�@��@�
=@�@�j@�n�@�V@�r�@�o@ϝ�@̴9@ˮ@�33@�n�@ɺ^@�O�@��`@Ǯ@���@��@Ĭ@�|�@\@��@�x�@�9X@��\@�?}@�C�@�n�@�^5@�E�@���@��@�Q�@��w@�
=@���@��D@�(�@���@�C�@���@���@�M�@�hs@���@��R@��@���@�p�@�V@���@�9X@��m@�ƨ@���@�;d@�n�@���@��-@�X@��/@�l�@���@�~�@�M�@�M�@�$�@�@��T@��^@�O�@�bN@��@�ȴ@���@�x�@���@�1@���@�@���@�v�@��@���@��@�V@�(�@��w@�o@�~�@�5?@�=q@���@��7@�G�@�?}@�7L@���@�Ĝ@�r�@�Q�@�(�@�bN@�V@��@��@��`@��u@�  @��;@��;@�dZ@�o@�o@�o@�v�@��7@�Ĝ@�Q�@�l�@��!@��@��@��@�{@�-@�5?@�=q@�=q@�V@��^@��`@��D@��@���@�x�@��T@��#@��#@�5?@��@��@�=q@�J@��-@��@�X@�@�-@�ȴ@�
=@�"�@�t�@�dZ@�\)@�S�@�K�@�C�@�"�@��@��\@�5?@��@��@���@�j@��;@�b@��@�Ĝ@��u@� �@��;@���@�t�@�@��+@�@���@���@�&�@��/@�bN@���@��w@�C�@���@��!@�
=@�\)@��\@�M�@�=q@�5?@�5?@�=q@�=q@�5?@�J@�$�@�5?@�5?@�J@��-@��h@�p�@�X@�?}@��@�Q�@�(�@���@��@��m@��@�l�@�;d@��@�
=@��@���@��!@�n�@�$�@�{@��@���@�X@��@���@�j@��@���@�+@��y@���@���@�~�@�^5@�$�@�@��@��-@��7@�G�@�V@���@�Ĝ@���@�r�@�A�@�1'@�;@�@~��@~V@~{@}�-@}�@|�D@|j@|(�@{��@{��@{dZ@z�@z�!@zJ@y��@yx�@x��@x�9@xbN@x1'@w��@w�P@w\)@w�@v�y@vȴ@vv�@v5?@u�@up�@t�@t�@t��@tI�@s�F@sC�@r�H@r�\@r^5@r=q@r-@q��@q�7@qX@qX@qX@qX@q7L@p��@p1'@o��@o
=@n�y@n�@nȴ@n��@nff@n@mp�@m?}@m�@l��@l�/@l�j@l�@lz�@k��@k�
@k�@k33@k@j��@j��@j�!@j��@j�\@j�\@j~�@j-@i�^@iX@h��@hĜ@h�u@h �@g�;@g�@gl�@gK�@f�y@fV@e�@d��@dj@cƨ@cS�@b�H@bn�@a��@a�#@ax�@a�@`�`@`�9@`Ĝ@`�`@`�u@`r�@`A�@_�;@_\)@^ȴ@^��@^ff@^{@]/@\��@\�/@\�/@\�/@\��@\�@\9X@[t�@["�@Z~�@ZJ@Y��@Y7L@Y�@X�`@X�9@X�u@X�@Xr�@XA�@W��@W|�@W;d@W
=@V�y@Vȴ@V��@V��@Vv�@Vff@V@Up�@T��@T�j@T�D@Tz�@TI�@T(�@S�m@S��@S33@R�H@R^5@Q��@Q��@Q7L@Q%@P�`@P��@P�9@PbN@PQ�@Pb@O�P@O�@N��@Nv�@M��@M`B@M/@M�@MV@L�@L��@Lj@K��@J��@J~�@Jn�@JM�@J=q@I��@I�7@Ix�@IX@I&�@I�@H�9@HA�@H  @G�@G+@F��@F��@F�@F�+@F{@E��@E��@E�@E`B@EO�@E/@D�@Dz�@D�@Cƨ@Ct�@C"�@Co@Co@C@B��@Bn�@BM�@BJ@A7L@@�9@@�@@bN@@1'@@ �@@b@?�;@?\)@>�@>��@>��@>�+@>ff@>E�@>$�@>$�@>$�@>@=��@=�@<z�@;"�@:M�@9�@9��@97L@8��@81'@8  @7�@7��@7��@7�P@7\)@7�@6�R@6��@6ff@6E�@5�@5/@4�@4�j@4��@4I�@3��@3�
@3��@3�@3dZ@3C�@333@3o@2�@2�@2�H@2��@2n�@2M�@2-@1��@1�#@1��@1&�@0�`@0�9@0bN@01'@0b@0  @0  @/�w@/|�@/\)@/+@.�R@.V@.{@-�@-@-��@-�h@-p�@-O�@-O�@-?}@,��@,�@,�D@,�D@,9X@+��@+�@+C�@*�@*�\@)�#@)x�@)G�@(��@(��@(bN@(A�@'�@';d@&�y@&��@&ff@&5?@&{@%@%�@$�@$(�@#�
@#��@#�@#dZ@#S�@#C�@#33@#@"�H@"��@"=q@!��@!�@!��@!hs@!&�@!%@ Ĝ@ �9@ ��@ �u@ �@ bN@   @��@�w@�@|�@l�@\)@;d@
=@�R@��@��@v�@V@E�@E�@5?@��@?}@V@�@��@��@j@9X@�@�
@�@@~�@M�@-@J@��@��@��@X@&�@Ĝ@��@r�@ �@��@�w@�w@�@��@�@ȴ@�R@�+@�+@�+@5?@�@?}@V@�@z�@Z@(�@�
@�@dZ@dZ@dZ@dZ@dZ@dZ@S�@�@�@�@�@�!@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�r�A�p�A�p�A�p�A�r�A�t�A�t�A�v�A��A��A��A��+A��+A��+A��DA��DA��DA��PA��\A��\A��PA��\A��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��A��A��!A��!A��!A��-A��9A��FA��9A��!A���A���A���A��A�VA��yA�;dA��+A���A���A���A��A��A��A��A�M�A�x�A���A���A��A� �A�5?A��A�1A���A��HA�JA���A��A�ƨA�9XA� �A�r�A��A�ƨA���A�1'A���A�n�A�$�A��`A��wA���A���A�1'A��A�A�K�A�/A���A�hsA��A�r�A�ĜA��`A�v�A�jA�1'A�VA���A���A��TA�Q�A�5?A��A���A~��A~�DA~bA|�A{��Az�Ay�Av�\Au%As�FAr��Ar�Aq�^Ao�TAl5?Aj1'Ai��Ah��Ag/Ae�-AdVAa|�A`bA]�7A[�hA[7LAZbNAX~�AU�mAUVAS��ARjAP1'AN�AL=qAJZAI�#AH  AG��AG�AEdZAC�
AAK�A@Q�A=�A<�A<E�A;��A;K�A;
=A:�RA:^5A9��A8ĜA8�A6��A4ȴA3?}A1�A0�A.E�A-��A,��A+�PA)��A(�uA'x�A&�A%7LA%�A$=qA"��A ĜAA��A��AbA��Ap�A=qA�TA��A^5A�AoAE�A��AbNA1A��A"�A��A+A��An�A{A�RA�mA?}A
bA=qAz�A�wA5?A�;A\)A�!AVAbA&�A   @�V@�z�@�9X@�l�@�@�?}@�j@��P@�v�@�@���@���@�r�@��H@�@�=q@�V@�w@���@�+@�+@��@�b@�{@��;@�$�@��@�
=@�@�j@�n�@�V@�r�@�o@ϝ�@̴9@ˮ@�33@�n�@ɺ^@�O�@��`@Ǯ@���@��@Ĭ@�|�@\@��@�x�@�9X@��\@�?}@�C�@�n�@�^5@�E�@���@��@�Q�@��w@�
=@���@��D@�(�@���@�C�@���@���@�M�@�hs@���@��R@��@���@�p�@�V@���@�9X@��m@�ƨ@���@�;d@�n�@���@��-@�X@��/@�l�@���@�~�@�M�@�M�@�$�@�@��T@��^@�O�@�bN@��@�ȴ@���@�x�@���@�1@���@�@���@�v�@��@���@��@�V@�(�@��w@�o@�~�@�5?@�=q@���@��7@�G�@�?}@�7L@���@�Ĝ@�r�@�Q�@�(�@�bN@�V@��@��@��`@��u@�  @��;@��;@�dZ@�o@�o@�o@�v�@��7@�Ĝ@�Q�@�l�@��!@��@��@��@�{@�-@�5?@�=q@�=q@�V@��^@��`@��D@��@���@�x�@��T@��#@��#@�5?@��@��@�=q@�J@��-@��@�X@�@�-@�ȴ@�
=@�"�@�t�@�dZ@�\)@�S�@�K�@�C�@�"�@��@��\@�5?@��@��@���@�j@��;@�b@��@�Ĝ@��u@� �@��;@���@�t�@�@��+@�@���@���@�&�@��/@�bN@���@��w@�C�@���@��!@�
=@�\)@��\@�M�@�=q@�5?@�5?@�=q@�=q@�5?@�J@�$�@�5?@�5?@�J@��-@��h@�p�@�X@�?}@��@�Q�@�(�@���@��@��m@��@�l�@�;d@��@�
=@��@���@��!@�n�@�$�@�{@��@���@�X@��@���@�j@��@���@�+@��y@���@���@�~�@�^5@�$�@�@��@��-@��7@�G�@�V@���@�Ĝ@���@�r�@�A�@�1'@�;@�@~��@~V@~{@}�-@}�@|�D@|j@|(�@{��@{��@{dZ@z�@z�!@zJ@y��@yx�@x��@x�9@xbN@x1'@w��@w�P@w\)@w�@v�y@vȴ@vv�@v5?@u�@up�@t�@t�@t��@tI�@s�F@sC�@r�H@r�\@r^5@r=q@r-@q��@q�7@qX@qX@qX@qX@q7L@p��@p1'@o��@o
=@n�y@n�@nȴ@n��@nff@n@mp�@m?}@m�@l��@l�/@l�j@l�@lz�@k��@k�
@k�@k33@k@j��@j��@j�!@j��@j�\@j�\@j~�@j-@i�^@iX@h��@hĜ@h�u@h �@g�;@g�@gl�@gK�@f�y@fV@e�@d��@dj@cƨ@cS�@b�H@bn�@a��@a�#@ax�@a�@`�`@`�9@`Ĝ@`�`@`�u@`r�@`A�@_�;@_\)@^ȴ@^��@^ff@^{@]/@\��@\�/@\�/@\�/@\��@\�@\9X@[t�@["�@Z~�@ZJ@Y��@Y7L@Y�@X�`@X�9@X�u@X�@Xr�@XA�@W��@W|�@W;d@W
=@V�y@Vȴ@V��@V��@Vv�@Vff@V@Up�@T��@T�j@T�D@Tz�@TI�@T(�@S�m@S��@S33@R�H@R^5@Q��@Q��@Q7L@Q%@P�`@P��@P�9@PbN@PQ�@Pb@O�P@O�@N��@Nv�@M��@M`B@M/@M�@MV@L�@L��@Lj@K��@J��@J~�@Jn�@JM�@J=q@I��@I�7@Ix�@IX@I&�@I�@H�9@HA�@H  @G�@G+@F��@F��@F�@F�+@F{@E��@E��@E�@E`B@EO�@E/@D�@Dz�@D�@Cƨ@Ct�@C"�@Co@Co@C@B��@Bn�@BM�@BJ@A7L@@�9@@�@@bN@@1'@@ �@@b@?�;@?\)@>�@>��@>��@>�+@>ff@>E�@>$�@>$�@>$�@>@=��@=�@<z�@;"�@:M�@9�@9��@97L@8��@81'@8  @7�@7��@7��@7�P@7\)@7�@6�R@6��@6ff@6E�@5�@5/@4�@4�j@4��@4I�@3��@3�
@3��@3�@3dZ@3C�@333@3o@2�@2�@2�H@2��@2n�@2M�@2-@1��@1�#@1��@1&�@0�`@0�9@0bN@01'@0b@0  @0  @/�w@/|�@/\)@/+@.�R@.V@.{@-�@-@-��@-�h@-p�@-O�@-O�@-?}@,��@,�@,�D@,�D@,9X@+��@+�@+C�@*�@*�\@)�#@)x�@)G�@(��@(��@(bN@(A�@'�@';d@&�y@&��@&ff@&5?@&{@%@%�@$�@$(�@#�
@#��@#�@#dZ@#S�@#C�@#33@#@"�H@"��@"=q@!��@!�@!��@!hs@!&�@!%@ Ĝ@ �9@ ��@ �u@ �@ bN@   @��@�w@�@|�@l�@\)@;d@
=@�R@��@��@v�@V@E�@E�@5?@��@?}@V@�@��@��@j@9X@�@�
@�@@~�@M�@-@J@��@��@��@X@&�@Ĝ@��@r�@ �@��@�w@�w@�@��@�@ȴ@�R@�+@�+@�+@5?@�@?}@V@�@z�@Z@(�@�
@�@dZ@dZ@dZ@dZ@dZ@dZ@S�@�@�@�@�@�!@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBn�Bn�Bn�Bn�Bn�Bn�Bn�Bn�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bm�Bn�Bn�Bn�Bm�Bn�Bn�Bo�Bp�Bp�Bo�Bp�Bp�Bn�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Bp�Bq�Bp�Bp�Bp�Bo�Bn�Bm�Bk�BiyBjBp�Bp�Bn�Bn�BXB-B�BuBJBB��B�`B�BǮB�jB�9B�B��B�7B� Bt�Bp�Bo�Bl�BffBS�BG�BE�B2-B�BuBPB	7BB  B�B��B�9B��B�uB�7B�%B� Bk�B]/BM�BC�B6FB0!B.B)�B�B%B
�yB
�B
��B
�dB
��B
}�B
o�B
m�B
iyB
aHB
ZB
O�B
H�B
9XB
/B
%�B
 �B
�B
�B
JB	��B	�B	�`B	�;B	�
B	��B	ĜB	�LB	�B	��B	��B	��B	�bB	�+B	{�B	v�B	q�B	jB	`BB	W
B	L�B	D�B	A�B	:^B	7LB	33B	,B	!�B	uB	\B	+B	B	  B��B��B��B��B��B�B�B�mB�HB�B��B��BÖB�qB�^B�LB�-B�B��B��B��B��B��B��B��B�{B�hB�\B�VB�JB�7B�+B�B�B�B�B�B� B}�B{�Bx�Bw�Bw�Bv�Bv�Bw�Bv�Bv�Bu�Bt�Br�Bp�Bn�Bk�BjBjBk�BjBjBjBjBiyBjBl�Bn�Bp�Bp�Bp�Br�Br�Br�Bs�Bu�Bu�Bu�Bu�Bu�Bt�Br�Bq�Bq�Bp�Bp�Bp�Bq�Bq�Bo�Bn�Bv�Bx�Bz�By�Bx�Bw�Bx�Bz�By�Bx�Bv�Bw�Bw�Bw�Bx�Bx�Bx�Bw�Bt�Bs�Bs�Bt�Bw�Bx�Bz�B|�B{�Bx�By�B}�B� B� B� B�B�B�B�%B�1B�DB�bB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�3B�9B�?B�LB�RB�}BÖBĜBŢBŢBƨBƨBǮBȴB��B��B��B�#B�BB�NB�`B�yB�B�B�B�B��B��B��B	  B	%B	1B	
=B	PB	\B	oB	�B	�B	�B	�B	�B	�B	 �B	,B	-B	0!B	7LB	A�B	D�B	H�B	H�B	H�B	H�B	H�B	H�B	G�B	G�B	G�B	F�B	E�B	B�B	@�B	?}B	=qB	=qB	=qB	>wB	?}B	A�B	B�B	C�B	C�B	C�B	D�B	F�B	G�B	H�B	K�B	O�B	ZB	aHB	bNB	cTB	iyB	r�B	t�B	s�B	s�B	t�B	t�B	y�B	�B	�+B	�VB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�?B	�FB	�LB	�XB	�XB	�^B	�^B	�XB	�RB	�dB	�}B	��B	��B	B	ĜB	ƨB	ɺB	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�;B	�HB	�NB	�NB	�TB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
	7B

=B
DB
JB
JB
PB
PB
VB
VB
VB
\B
\B
bB
bB
hB
hB
hB
oB
oB
oB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
(�B
(�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
.B
.B
/B
/B
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
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
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
:^B
;dB
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
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
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
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
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
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
XB
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
YB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
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
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
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
hsB
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
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bn�Bn�Bn�Bn�Bn�Bn�Bn�BnoBm�Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bm�Bn�Bn�Bn�Bm�Bn�Bn�Bo�Bp�Bp�Bo�Bp�Bp�Bn�Bn�Bn�Bn�Bn�Bn�Bo�Bo�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bp�Bq�Bp�Bq�Bp�Bp�Bp�Bo�Bo%Bo�BlEBl�Bv Bs0BqMBo�Bt�Bc�B4�B>B\BFB>B�kB��B�0B�_B�\B�ZB�B��B��B��B|BqgBrBq�Bm�BV^BH�BJ�B8�B�B�B�B
nB	B�B��BՃB��B�aB��B��B��B��Bp?Bb�BQ�BH=B8{B0�B/�B/�B>BBB
�B
�DB
��B
ǾB
�B
�aB
pyB
oB
l�B
d�B
^0B
R�B
OZB
=tB
2�B
(@B
"�B
�B
1B
B	�B	��B	�IB	�B	��B	�wB	��B	�'B	��B	��B	�aB	�QB	�IB	��B	}�B	ySB	uVB	ofB	dB	\�B	QB	E�B	F B	;mB	8�B	7�B	0B	(fB	GB	�B		�B	�B	�B��B��B��B��B��B��B�gB�B�qB�;B�iBφB�eB�B��B��B�~B��B��B�EB��B�B��B��B��B��B�$B��B��B�+B�KB��B�B��B�B�'B�B��B�B~�By�Bx�By3Bz~BxBx�Bw�Bw�By:Bv�Bt}Bs�BsOBp^Bl�Bn�Bl�Bk�Bl[Bk�BknBlBm�BoBq3Bq#Bq�Br�Bs�Bs�BtBuIBvqBvBv0BwyBw�Bw�Bu�Bs]Bs�Bt�Bs#Bq�Br�Bt	BrgBq�By[B{�B|BB{sB{=Bz�Bz�B{�B|)B}�Bz�ByhBx�ByBy�By�BymByVBw
Bt�BtcBvQBx�By�B{�B~�B~Bz�B|kBB�.B�=B��B�B�-B��B�BB��B�
B� B�B�GB�/B��B�B��B��B�BB��B�kB��B��B��B��B��B�WB�hB��B�TB��B��B��B�4B�|B��B��B��B��B��B��B��B�B�tB�@BѾB��B�hB�.B�B�\B�GB�B�IB�B�_B�ZB�mB��B	KB	�B		B	B	�B	oB	�B	2B	B	�B	�B	�B	B	!JB	,QB	-[B	/�B	6�B	A�B	D�B	H�B	IGB	I�B	H�B	H�B	IvB	H8B	G�B	G�B	G�B	GB	C�B	AGB	@�B	>�B	>OB	=�B	>�B	?mB	A�B	B�B	C�B	C�B	C�B	E�B	G�B	H7B	H�B	K}B	OB	Y�B	anB	bdB	b�B	h�B	r�B	u�B	tB	tKB	uB	uB	y^B	��B	�sB	�B	�fB	�B	��B	��B	��B	��B	��B	�B	�'B	��B	�~B	��B	��B	�@B	��B	��B	��B	�?B	�"B	��B	�B	��B	��B	�B	�,B	�>B	��B	��B	��B	�HB	�B	�]B	�KB	�B	ǅB	ʁB	�B	�uB	ӨB	�`B	ؓB	�NB	�BB	�8B	�0B	�@B	�TB	݂B	�$B	�=B	�fB	�B	��B	�B	�B	�B	�B	��B	�kB	��B	��B	�B	�B	��B	� B	��B	��B	��B	��B	��B	��B	�B	�,B	��B	��B	�;B	�HB	�"B	�.B	�NB	�hB	�GB	�wB	�GB	�4B	�2B	�=B	�AB
 dB
IB
>B
iB
\B
zB
yB
JB
~B
nB
lB
�B
[B
�B
�B
	�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!B
B
B
�B
�B
�B
�B
�B
B
&B
�B
�B
�B
�B
�B
�B
�B
$B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
 B
 -B
!*B
".B
"B
"B
"9B
#B
#B
#B
#B
#7B
#WB
$�B
#�B
#B
#`B
#DB
#?B
#<B
$CB
$B
$:B
%=B
%B
%B
$�B
%�B
'=B
'B
''B
'NB
(gB
)zB
)4B
*=B
*WB
*�B
*;B
*/B
*B
*B
*"B
)-B
)dB
*�B
+XB
+�B
+pB
+eB
+iB
,9B
,FB
,BB
-@B
-9B
-9B
-OB
.{B
.jB
/eB
/SB
0RB
0VB
1\B
1OB
1]B
1OB
1�B
2�B
2�B
3|B
3qB
3YB
3tB
3kB
3{B
4�B
4�B
4�B
4�B
5�B
5�B
5�B
6�B
6}B
6mB
6zB
6�B
6pB
6�B
6�B
6�B
6�B
6�B
6�B
7�B
7�B
8zB
8|B
8�B
8�B
8�B
9B
:B
9�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A8B
BB
B�B
B�B
B�B
B�B
B�B
B�B
CB
DB
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
E�B
F�B
GfB
HB
G�B
H#B
IEB
I$B
I�B
I�B
I�B
I�B
J�B
KB
KB
K)B
K�B
L	B
K�B
L&B
L�B
MB
MB
MB
M,B
M(B
N
B
NB
NB
NB
NB
N B
NB
OB
N�B
OB
OB
O5B
OB
OB
P#B
PB
P%B
PdB
Q8B
Q*B
QAB
Q'B
QB
RB
RB
R>B
R?B
R#B
R4B
RlB
S^B
SBB
S+B
S9B
S+B
T"B
T.B
T/B
TB
T&B
TPB
TYB
T0B
TB
TaB
T�B
U9B
UUB
UbB
UxB
V�B
VtB
WTB
WrB
WoB
WbB
XQB
W�B
X�B
XtB
YnB
YnB
YcB
YSB
Y|B
Y{B
Y�B
Z�B
[�B
[jB
[[B
[]B
[NB
\SB
[NB
\uB
\cB
\�B
\�B
\}B
][B
]�B
]�B
]�B
]gB
]�B
^`B
^`B
^cB
^aB
^sB
^�B
^{B
^`B
^_B
^|B
_hB
_hB
^oB
_�B
_�B
_eB
_ZB
_B
_sB
_cB
_VB
_jB
_�B
`�B
`�B
`yB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
c~B
c�B
d�B
d�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
e�B
e�B
f�B
f�B
f�B
g�B
g�B
g�B
hB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
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
j�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Ix&<#�
<#�
<#�
<#�
<K�'<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<?�%<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<_Qk<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<5^<#�
<#�
<#�
<U�^<H�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.21 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101134362016031011343620160310113436  AO  ARCAADJP                                                                    20140721233845    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721233845  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721233845  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113436  QC  PRES            @�33D칚G�O�                PM  ARSQCTM V1.1                                                                20160310113436  QC  PSAL            @�33D칚G�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133323  IP                  G�O�G�O�G�O�                