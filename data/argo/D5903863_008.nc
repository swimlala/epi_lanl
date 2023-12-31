CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-07-21T23:38:47Z creation; 2014-07-21T23:38:47Z updated; 2015-09-28T12:13:22Z converted from 3.0   
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
resolution        =���   axis      Z        H  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     H  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  o(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     H  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721233847  20170523133323  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  4298_0127_008                   2C  D   NAVIS_A                         0127                            120111                          863 @�3�i@1   @�3���@5:�G�{�d�^5?|�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D���D�@ Dу3D��3D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�3D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@�G�@�G�A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�Dur�Du�Dvr�Dv�Dwr�Dw�Dxr�Dx�Dyr�Dy�Dzr�Dz�D{r�D{�D|r�D|�D}r�D}�D~r�D~�Dr�D�D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�<{D�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD¹HD��HD�9HD�yHDùHD��HD�9HD�yHDĹHD��HD�9HD�yHDŹHD��HD�9HD�yHDƹHD��HD�9HD�yHDǹHD��HD�9HD�yHDȹHD��HD�9HD�yHDɹHD��HD�9HD�yHDʹHD��HD�9HD�yHD˹HD��HD�9HD�yHD̹HD��HD�9HD�yHD͹HD��HD�9HD�yHDιHD��HD�9HD�yHDϹHD��HD�9HD�yHDйHD��D�9HD�|{DѼ{D��HD�9HD�yHDҹHD��HD�9HD�yHDӹHD��HD�9HD�yHDԹHD��HD�9HD�yHDչHD��HD�9HD�yHDֹHD��HD�9HD�yHD׹HD��HD�9HD�yHDعHD��HD�9HD�yHDٹHD��HD�9HD�yHDڹHD��HD�9HD�yHD۹HD��HD�9HD�yHDܹHD��{D�9HD�yHDݹHD��HD�9HD�yHD޹HD��HD�9HD�yHD߹HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�{D��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�|{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�5?A�5?A�5?A�33A�33A�33A�9XA�9XA�7LA�9XA�9XA�7LA�7LA�7LA�;dA�;dA�?}A�=qA�A�A�C�A�C�A�A�A�=qA�5?A�+A�+A�bA�%A���A��A��mA��HA��#A��#A��#A��A���A�ƨA��-A��uA�v�A��`A�~�A�$�A��wA�ZA�9XA�A�A�|�A��PA���A���A���A��\A�\)A�A�A�bA��A�~�A�$�A�v�A���A�S�A�ZA���A��mA�jA��\A��A�r�A�%A���A��A�K�A�9XA�|�A�v�A�/A�C�A�^5A�O�A�\)A��DA���A��PA��A��;A�^5A���A�O�A��-A���A���A���A�1'A�ȴA�I�A��A��DA�JA��A�t�A�l�A���A���A�(�A��9A�A��DA���A��mA���A�VA��9A�`BA`BA}%A{`BAy�Ax�9AvA�At~�As`BAqAoO�Ak+Ah-AfZAcXAa�hAaXA`��A_�A_"�A^VA\��AZ�+AWVAT$�ARȴAR-AQl�AP  AN��AN=qAMO�AHv�AFȴAC�7AA\)A@��A@jA?C�A=
=A<~�A<1'A<$�A< �A<A;�wA:�A8��A5�-A4JA2��A1��A1XA0M�A/t�A/;dA/oA.�jA-33A+�A*ZA(�`A(^5A'?}A&�A&�A&-A$��A"��A ��A 1'A`BAVA�At�A��AI�AM�A�HA�AI�AXA�AM�A��A �A �A��AdZA��AE�At�A�+A��A33AA	�7A�DAA�A�A�AK�A5?Ap�A\)Ax�A7LA�`A�A M�@��R@�$�@�hs@��@�ƨ@��@��-@�|�@�@�P@�v�@��@��@�p�@�%@�D@���@�@�D@�ȴ@�^@�h@�hs@�&�@�j@���@��D@�~�@ۅ@�J@�x�@׮@�1@�\)@���@�C�@�V@��
@˕�@��@���@ɉ7@�hs@���@� �@�^5@���@�O�@�ȴ@��#@�&�@�I�@�;d@���@��
@�
=@�5?@�x�@��@�&�@���@��j@�(�@�l�@�33@��@�p�@�Ĝ@�9X@�|�@�=q@���@��#@��^@�X@�&�@��u@�Q�@�1'@���@��;@��
@��@�|�@���@��\@��^@���@��/@�Q�@��w@���@�^5@��@��/@�9X@��w@�dZ@���@�J@���@�&�@��`@�z�@��w@��@��!@���@��@�7L@���@��D@�Z@� �@��;@���@�\)@�33@�
=@��H@�^5@�5?@�J@���@���@�ff@��\@�p�@��D@�%@�/@�/@��m@�ȴ@�z�@�(�@��@�@��!@���@���@���@�n�@�^5@�E�@�5?@�-@��@�@�J@�M�@��@��@��@�r�@�I�@�Q�@��/@�V@��`@��
@�t�@�S�@�K�@�C�@�33@�"�@�o@��R@�E�@�J@���@�`B@�?}@���@���@��/@��@���@�z�@�Q�@�9X@�A�@�bN@��@���@���@���@��P@��P@���@��P@�|�@�S�@��@�o@��@��@�"�@��@��@�-@���@���@��T@���@��^@��^@�`B@��7@��@���@��h@�G�@�Ĝ@�z�@�  @��
@�ƨ@���@��@��@�\)@�;d@�o@���@���@�ff@�-@�@�J@�@��T@�@���@�/@��@�V@��@���@�bN@�I�@��@���@��
@��w@��@�;d@�
=@���@��@��y@�ȴ@��\@��#@��h@�x�@�G�@���@�Ĝ@���@�j@�bN@�Z@�Q�@�A�@��m@�l�@�l�@�C�@�;d@�o@�@��R@��@�p�@�G�@�/@���@��@� �@\)@;d@;d@~��@~V@~$�@}��@}�@~@}�@}@}�@|��@|�@|z�@{��@{t�@{33@z�!@z^5@z=q@z-@z-@y�^@y&�@x�`@x�@xb@w\)@v��@v��@vv�@vff@vV@v$�@u�T@u�-@u`B@t�@tI�@tI�@t9X@sƨ@r�H@r=q@q��@q�#@rJ@r=q@rn�@r=q@q��@qx�@q�@qG�@q&�@p�9@p�@p�@pQ�@p  @o��@ol�@o
=@n{@mp�@m?}@m/@mV@l��@lZ@l(�@kƨ@kt�@j=q@ihs@iX@i�@h�u@h�@hbN@hQ�@hA�@h1'@hb@g��@g�@g��@g|�@g|�@g|�@g|�@gl�@f�y@f�R@f��@fV@f@e?}@d��@d�@d1@cƨ@c�F@c��@cS�@co@b^5@a��@a��@a&�@`��@`r�@_�@_�@_l�@_\)@^�y@^ff@]��@]p�@]`B@]O�@]?}@]/@]/@\��@\z�@\(�@[��@Z��@Z~�@Zn�@ZJ@Y��@Yhs@YX@Y%@XA�@W��@W�P@W;d@V�@V��@VE�@U�@U�T@U��@T��@T�@SdZ@S"�@R~�@Rn�@R-@Q��@Q��@Q��@QX@Q&�@Q�@P��@P�9@P�@PbN@Pb@O�@O�w@O\)@N��@N��@N��@N5?@M�@M�@MV@L�@L�@L�@L�@L��@K�F@K33@Ko@J�@J�!@J=q@I�@Ix�@I%@H�9@H�@HQ�@H  @G�w@G�P@F��@F�@F�+@FE�@E@E��@EO�@E?}@EV@D�j@D9X@D1@Cƨ@CdZ@C33@C"�@Co@B�@B��@B��@B��@B�!@B^5@A��@A��@A��@Ahs@A�@@��@@��@@Q�@@  @?��@?��@?|�@>�y@>ȴ@>v�@=�@=�@=?}@=�@<�/@<1@;�F@;t�@:�@:^5@:�@9�#@97L@8��@8 �@7�@7�w@7;d@6��@6��@6��@6V@5@4�@41@3�m@3��@3t�@3dZ@2�@2-@1�@1��@1�7@1G�@0��@0Ĝ@0�9@0r�@0A�@01'@01'@01'@01'@/�@/�P@/;d@/+@.��@.ȴ@.v�@.{@-@-?}@,��@,�@,�/@,�j@,Z@+��@+�
@+�F@+��@+�@+S�@+"�@*��@*M�@)��@)�7@)X@)&�@(��@(�@(b@'�;@'��@'|�@'\)@';d@'�@&�R@&{@%�@%��@%�-@%�@%O�@$��@$��@$j@$Z@#�m@#ƨ@#��@#t�@#dZ@#C�@#"�@#o@"�@"�\@"�@!�#@!��@!�7@!G�@!�@ �`@ ��@ ��@ Ĝ@ �9@ �u@ A�@�;@��@K�@��@�@��@@@�-@�h@`B@�@��@�@�@��@I�@�m@ƨ@�F@��@�@C�@33@o@��@��@�@��@7L@%@��@�9@r�@Q�@Q�@Q�@Q�@Q�@1'@�@�;@��@�@|�@K�@�+@@�h@/@�@��@�/@�j@z�@9X@��@�F@dZ@"�@o@@�@��@�!@M�@-@��@�@�#@��@�^@��@G�@%@�`@Ĝ@r�@Q�@Q�@1'@�@��@��@��@�P@|�@�@��@ff@E�@$�@{@@�h@`B@O�@/@�@V@��@��@��@z�@j@�@��@C�@33@o@@
��@
�!@
=q@	��@	��@	X@	7L@	�@	%@��@�`@Ĝ@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�5?A�5?A�5?A�33A�33A�33A�9XA�9XA�7LA�9XA�9XA�7LA�7LA�7LA�;dA�;dA�?}A�=qA�A�A�C�A�C�A�A�A�=qA�5?A�+A�+A�bA�%A���A��A��mA��HA��#A��#A��#A��A���A�ƨA��-A��uA�v�A��`A�~�A�$�A��wA�ZA�9XA�A�A�|�A��PA���A���A���A��\A�\)A�A�A�bA��A�~�A�$�A�v�A���A�S�A�ZA���A��mA�jA��\A��A�r�A�%A���A��A�K�A�9XA�|�A�v�A�/A�C�A�^5A�O�A�\)A��DA���A��PA��A��;A�^5A���A�O�A��-A���A���A���A�1'A�ȴA�I�A��A��DA�JA��A�t�A�l�A���A���A�(�A��9A�A��DA���A��mA���A�VA��9A�`BA`BA}%A{`BAy�Ax�9AvA�At~�As`BAqAoO�Ak+Ah-AfZAcXAa�hAaXA`��A_�A_"�A^VA\��AZ�+AWVAT$�ARȴAR-AQl�AP  AN��AN=qAMO�AHv�AFȴAC�7AA\)A@��A@jA?C�A=
=A<~�A<1'A<$�A< �A<A;�wA:�A8��A5�-A4JA2��A1��A1XA0M�A/t�A/;dA/oA.�jA-33A+�A*ZA(�`A(^5A'?}A&�A&�A&-A$��A"��A ��A 1'A`BAVA�At�A��AI�AM�A�HA�AI�AXA�AM�A��A �A �A��AdZA��AE�At�A�+A��A33AA	�7A�DAA�A�A�AK�A5?Ap�A\)Ax�A7LA�`A�A M�@��R@�$�@�hs@��@�ƨ@��@��-@�|�@�@�P@�v�@��@��@�p�@�%@�D@���@�@�D@�ȴ@�^@�h@�hs@�&�@�j@���@��D@�~�@ۅ@�J@�x�@׮@�1@�\)@���@�C�@�V@��
@˕�@��@���@ɉ7@�hs@���@� �@�^5@���@�O�@�ȴ@��#@�&�@�I�@�;d@���@��
@�
=@�5?@�x�@��@�&�@���@��j@�(�@�l�@�33@��@�p�@�Ĝ@�9X@�|�@�=q@���@��#@��^@�X@�&�@��u@�Q�@�1'@���@��;@��
@��@�|�@���@��\@��^@���@��/@�Q�@��w@���@�^5@��@��/@�9X@��w@�dZ@���@�J@���@�&�@��`@�z�@��w@��@��!@���@��@�7L@���@��D@�Z@� �@��;@���@�\)@�33@�
=@��H@�^5@�5?@�J@���@���@�ff@��\@�p�@��D@�%@�/@�/@��m@�ȴ@�z�@�(�@��@�@��!@���@���@���@�n�@�^5@�E�@�5?@�-@��@�@�J@�M�@��@��@��@�r�@�I�@�Q�@��/@�V@��`@��
@�t�@�S�@�K�@�C�@�33@�"�@�o@��R@�E�@�J@���@�`B@�?}@���@���@��/@��@���@�z�@�Q�@�9X@�A�@�bN@��@���@���@���@��P@��P@���@��P@�|�@�S�@��@�o@��@��@�"�@��@��@�-@���@���@��T@���@��^@��^@�`B@��7@��@���@��h@�G�@�Ĝ@�z�@�  @��
@�ƨ@���@��@��@�\)@�;d@�o@���@���@�ff@�-@�@�J@�@��T@�@���@�/@��@�V@��@���@�bN@�I�@��@���@��
@��w@��@�;d@�
=@���@��@��y@�ȴ@��\@��#@��h@�x�@�G�@���@�Ĝ@���@�j@�bN@�Z@�Q�@�A�@��m@�l�@�l�@�C�@�;d@�o@�@��R@��@�p�@�G�@�/@���@��@� �@\)@;d@;d@~��@~V@~$�@}��@}�@~@}�@}@}�@|��@|�@|z�@{��@{t�@{33@z�!@z^5@z=q@z-@z-@y�^@y&�@x�`@x�@xb@w\)@v��@v��@vv�@vff@vV@v$�@u�T@u�-@u`B@t�@tI�@tI�@t9X@sƨ@r�H@r=q@q��@q�#@rJ@r=q@rn�@r=q@q��@qx�@q�@qG�@q&�@p�9@p�@p�@pQ�@p  @o��@ol�@o
=@n{@mp�@m?}@m/@mV@l��@lZ@l(�@kƨ@kt�@j=q@ihs@iX@i�@h�u@h�@hbN@hQ�@hA�@h1'@hb@g��@g�@g��@g|�@g|�@g|�@g|�@gl�@f�y@f�R@f��@fV@f@e?}@d��@d�@d1@cƨ@c�F@c��@cS�@co@b^5@a��@a��@a&�@`��@`r�@_�@_�@_l�@_\)@^�y@^ff@]��@]p�@]`B@]O�@]?}@]/@]/@\��@\z�@\(�@[��@Z��@Z~�@Zn�@ZJ@Y��@Yhs@YX@Y%@XA�@W��@W�P@W;d@V�@V��@VE�@U�@U�T@U��@T��@T�@SdZ@S"�@R~�@Rn�@R-@Q��@Q��@Q��@QX@Q&�@Q�@P��@P�9@P�@PbN@Pb@O�@O�w@O\)@N��@N��@N��@N5?@M�@M�@MV@L�@L�@L�@L�@L��@K�F@K33@Ko@J�@J�!@J=q@I�@Ix�@I%@H�9@H�@HQ�@H  @G�w@G�P@F��@F�@F�+@FE�@E@E��@EO�@E?}@EV@D�j@D9X@D1@Cƨ@CdZ@C33@C"�@Co@B�@B��@B��@B��@B�!@B^5@A��@A��@A��@Ahs@A�@@��@@��@@Q�@@  @?��@?��@?|�@>�y@>ȴ@>v�@=�@=�@=?}@=�@<�/@<1@;�F@;t�@:�@:^5@:�@9�#@97L@8��@8 �@7�@7�w@7;d@6��@6��@6��@6V@5@4�@41@3�m@3��@3t�@3dZ@2�@2-@1�@1��@1�7@1G�@0��@0Ĝ@0�9@0r�@0A�@01'@01'@01'@01'@/�@/�P@/;d@/+@.��@.ȴ@.v�@.{@-@-?}@,��@,�@,�/@,�j@,Z@+��@+�
@+�F@+��@+�@+S�@+"�@*��@*M�@)��@)�7@)X@)&�@(��@(�@(b@'�;@'��@'|�@'\)@';d@'�@&�R@&{@%�@%��@%�-@%�@%O�@$��@$��@$j@$Z@#�m@#ƨ@#��@#t�@#dZ@#C�@#"�@#o@"�@"�\@"�@!�#@!��@!�7@!G�@!�@ �`@ ��@ ��@ Ĝ@ �9@ �u@ A�@�;@��@K�@��@�@��@@@�-@�h@`B@�@��@�@�@��@I�@�m@ƨ@�F@��@�@C�@33@o@��@��@�@��@7L@%@��@�9@r�@Q�@Q�@Q�@Q�@Q�@1'@�@�;@��@�@|�@K�@�+@@�h@/@�@��@�/@�j@z�@9X@��@�F@dZ@"�@o@@�@��@�!@M�@-@��@�@�#@��@�^@��@G�@%@�`@Ĝ@r�@Q�@Q�@1'@�@��@��@��@�P@|�@�@��@ff@E�@$�@{@@�h@`B@O�@/@�@V@��@��@��@z�@j@�@��@C�@33@o@@
��@
�!@
=q@	��@	��@	X@	7L@	�@	%@��@�`@Ĝ@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�)B�)B�)B�#B�)B�#B�#B�#B�#B�#B�#B�)B�)B�5B�/B�HB�NB�NB�TB�`B�fB�mB�mB�mB�sB�yB�B�B�B�B��B��B��B��B��B��B��BDB\BuB{B�B�B�B�B�B�B�B!�B+B.B)�B�BB��B��B�BB��BǮB��B�3B��B��B��B��B�uB�Bq�B^5BP�BE�B=qB49B0!B$�B�BB��B�B�sB�BȴB�XB�'B��B�bB�BcTBL�BE�BC�BB�B:^B(�BDB
�mB
�B
��B
��B
��B
�+B
z�B
u�B
o�B
\)B
E�B
9XB
/B
"�B
VB
  B	��B	�B	�)B	��B	ȴB	�^B	��B	��B	��B	��B	��B	��B	��B	�1B	u�B	]/B	K�B	A�B	:^B	2-B	&�B	�B	�B	hB	1B	B��B�B�yB�fB�ZB�fB�mB�mB�mB�fB�`B�ZB�BB�B��BƨBB��B�}B�jB�^B�XB�RB�FB�-B�B��B��B��B��B��B��B��B�{B�PB�%B�B� B|�Bx�Bv�Bs�Bp�BjBe`BdZBbNB`BB^5B]/B[#BW
BS�BR�BQ�BO�BM�BL�BL�BL�BJ�BH�BH�BI�BH�BN�BT�BQ�BN�BO�BVBcTBgmBffBdZBcTBdZBcTBbNBaHB_;B\)BZBYBYBYBZBYBYBYBYBXBXBXBXBYBZBZBZBYBXBYBZB\)B`BBbNBaHBbNBe`BdZBcTBcTBdZBe`Be`Be`BffBffBffBe`Be`BgmBgmBffBiyBiyBk�Bm�Bn�Bq�Bw�By�B� B�B�B�1B�=B�JB�JB�JB�PB�VB�{B��B��B��B��B��B��B��B�B�B�-B�9B�?B�FB�RB�XB�^B�jB�jB�jB��BŢBŢBƨBǮBǮBɺB��B��B��B��B��B�B�;B�HB�TB�ZB�fB�B�B�B��B��B��B��B	  B	B	%B	
=B	PB	oB	{B	�B	�B	�B	�B	�B	�B	 �B	&�B	(�B	(�B	+B	/B	33B	6FB	49B	2-B	1'B	49B	8RB	=qB	A�B	C�B	D�B	D�B	H�B	J�B	K�B	L�B	L�B	N�B	O�B	Q�B	W
B	YB	ZB	]/B	_;B	bNB	dZB	hsB	jB	iyB	jB	m�B	o�B	p�B	q�B	r�B	s�B	s�B	u�B	y�B	{�B	~�B	�B	�B	�1B	�PB	�oB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�-B	�3B	�9B	�?B	�FB	�LB	�^B	�jB	�jB	�wB	�wB	��B	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�/B	�5B	�;B	�BB	�BB	�BB	�ZB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
%B
%B
+B
+B
	7B
DB
DB
JB
JB
JB
PB
VB
\B
\B
\B
bB
bB
bB
bB
bB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
hB
hB
hB
hB
hB
hB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
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
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
+B
,B
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
0!B
1'B
0!B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
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
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
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
>wB
>wB
>wB
>wB
>wB
?}B
?}B
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
@�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
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
I�B
I�B
J�B
J�B
J�B
J�B
K�B
J�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
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
XB
XB
XB
XB
XB
XB
XB
XB
YB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
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
_;B
_;B
_;B
`BB
`BB
`BB
aHB
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
bNB
bNB
bNB
bNB
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
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
hsB
hsB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
jB
jB
iyB
jB
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
l�B
l�B
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
o�B
o�B
o�B
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
r�B
r�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�7B�7B�@B�5B�7B�B�8B�@B�-B�8B�BB�9B�=B�*B�6B�'B�>B�%B�,B�6B�DB�NB�lB�tB�PB��B�B�B�B�B�B�B�B�B�B�B��B�B�AB�]B�B��B��B��B��B�aB��B�BB5BvB�B�BeBBYBB�B�B$�B- B0�B0�BBqB�SB��B��B�XB�=B�jB��B��B�/B��B�hB�|B��By�Bb�BT�BI?B@�B5kB2�B)�B BB��B�oB�B�1B̠B�DB��B��B�tB��Bi�BO6BE�BC�BD�B?GB1aBB
�B
�B
ʾB
��B
��B
�B
|�B
w�B
xgB
bPB
I�B
<�B
2LB
(zB
BB
{B	�VB	�BB	�UB	��B	�B	��B	��B	��B	�B	��B	�SB	�FB	�B	��B	}B	c�B	N�B	CB	<5B	5{B	)�B	 %B	)B	SB	6B		�B��B�OB�:B�QB�B��B�<B�B�B��B�-B�B�BߞB��B��B�&B��B�5B��B�B��B�uB�JB�MB��B��B�WB��B��B��B�'B��B�	B�nB�OB�QB�B�Bz.Bx4Bu�Bv	Bn-BfBe�Bd�BbsB^�B_FB_ B\fBT�BT@BS2BQ�BPBOPBN�BN"BMmBM�BJ�BJ�BK�BO�BW�BT�BP�BP$BU�BdBhVBh�Bh�Be�Be2BdUBcBb�Bb�BaDB]$B[�B\BZ�BZ�BY}BY�BY�BY�BYB[]BY�BZ�BZ�BZsBZvBZ�BZKBZ�B\MB]*B`6BbNBcABc�BghBfdBf�Bf�Bf�BfBe�Bf-BgJBf�Bf�BgBf�Bg�BhOBhGBi�Bj�Bj�Bl�BoBpTBs�Bx�Bz�B��B��B�3B�B��B�B�;B��B��B� B�wB�^B��B��B�:B�'B�GB��B�qB�B��B��B��B��B��B��B��B�UB�FB��B��B��BƟBǯB�B��BʞBΏB��B��BӞB�B�IB��B�'B��B�B�B�B�VB��B��B�]B�xB�tB	 _B	oB	�B	
�B	�B	�B	�B	�B	YB	�B	�B	B	�B	 B	&�B	*�B	*?B	*oB	.�B	3cB	8CB	6B	5�B	1�B	5DB	9*B	>
B	A�B	C�B	D�B	D�B	H�B	J�B	K�B	L�B	L�B	OB	O�B	Q�B	X�B	Y�B	Z:B	]�B	_�B	bZB	c�B	hUB	j�B	j�B	kB	m�B	o�B	p�B	q�B	r�B	s�B	t\B	v�B	zUB	|�B	kB	�UB	��B	�yB	�[B	�zB	�B	��B	��B	��B	��B	�xB	��B	�B	��B	��B	��B	��B	��B	��B	�B	�DB	�XB	�B	�B	�6B	�0B	�NB	��B	�nB	��B	�VB	��B	�yB	��B	�B	��B	�NB	��B	�iB	��B	�B	�eB	�6B	�zB	�!B	�B	�B	�B	��B	�.B	�'B	�>B	�dB	�OB	�oB	�gB	�ZB	�*B	�BB	�iB	�rB	݂B	��B	�mB	�uB	��B	�B	��B	�B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	��B	��B	�uB	�B	��B	�B	�EB	�$B	�B	�.B	� B	�B	�B	�B	�mB	��B	��B	�5B	�B	�@B	�)B	�{B	�B	��B	�7B	�$B	�WB	��B	��B	��B	�B	�B	�zB	�CB
 >B
XB
B
"B
PB
fB
vB
�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
6B
 B
�B
�B
dB
mB
xB
�B
 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
KB
#B
�B
�B
�B
B
�B
�B
 B
�B
�B
MB
�B
�B
B
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
!7B
 �B
 �B
!B
!B
"sB
"B
" B
"XB
#B
"�B
"�B
#*B
#B
#mB
$\B
$B
$FB
$2B
%@B
%UB
&.B
'3B
'B
']B
'eB
(�B
(+B
)!B
)B
)"B
) B
)B
)[B
)RB
*YB
*|B
*�B
+QB
+/B
+jB
+rB
+DB
+.B
+\B
+�B
,yB
-aB
-iB
.xB
.bB
.mB
.iB
.<B
.dB
.�B
/�B
0�B
1vB
0�B
1PB
2}B
2nB
2kB
2bB
2�B
3rB
3XB
3fB
4�B
4wB
5sB
5�B
5rB
5B
5�B
6�B
6�B
6qB
6�B
7�B
7�B
7�B
8�B
8pB
8mB
8�B
8�B
9B
8�B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
;�B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
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
@�B
@�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
EB
D�B
D�B
EB
FB
E�B
F�B
F�B
G^B
GB
H B
H2B
H5B
IB
IB
ITB
IFB
J4B
I�B
KB
KEB
KB
KB
K�B
KB
L<B
L�B
MHB
L�B
NB
NB
M�B
N5B
NfB
OB
OB
OB
P$B
P+B
PB
PB
P"B
PB
QB
Q B
QB
QB
Q-B
QDB
Q7B
RB
R*B
R-B
R>B
RNB
SDB
SgB
S9B
TB
T$B
T.B
T[B
TYB
T,B
T*B
U0B
U%B
U@B
UAB
U^B
U~B
V�B
VRB
WKB
WPB
WkB
WfB
W~B
WOB
W[B
XIB
XIB
XHB
XJB
X�B
X�B
XHB
XGB
XGB
XWB
XUB
XvB
X|B
XVB
YIB
X�B
YPB
Y]B
ZTB
ZEB
ZVB
ZUB
ZJB
Z\B
[�B
[�B
\{B
]_B
]�B
]�B
]xB
^zB
^^B
^WB
^`B
^bB
^pB
^�B
^�B
^�B
_�B
_�B
_uB
`�B
`�B
`�B
awB
`}B
a�B
a�B
a�B
a�B
ahB
axB
a�B
b�B
b�B
bxB
b{B
b�B
b�B
c�B
c�B
c�B
c}B
dB
d�B
d�B
d�B
d�B
e�B
e�B
e�B
exB
ezB
ezB
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
g-B
f�B
g�B
g�B
g�B
g�B
g�B
h�B
g�B
h�B
h�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
i�B
i�B
j�B
j�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
kB
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
n�B
n�B
n�B
n�B
o	B
o�B
o�B
o�B
p�B
p�B
p�B
qB
q	B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<M�<#�
<#�
<(��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<@�&<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.21 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101134372016031011343720160310113437  AO  ARCAADJP                                                                    20140721233847    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721233847  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721233847  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113437  QC  PRES            @���D��fG�O�                PM  ARSQCTM V1.1                                                                20160310113437  QC  PSAL            @���D��fG�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133323  IP                  G�O�G�O�G�O�                