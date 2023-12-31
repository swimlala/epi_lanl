CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-23T03:36:27Z creation;2018-07-23T03:36:32Z conversion to V3.1;2019-12-19T07:37:39Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180723033627  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_259                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�qu͎� 1   @�qv��>�@9�)�y���dN����1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz�fD{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�C3DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @%@r�\@�G�@�G�A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�AͅA�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/(�B7(�B?(�BG(�BO(�BVB^Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!��C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�Dur�Du�Dvr�Dv�Dwr�Dw�Dxr�Dx�Dyr�Dy�Dzx�Dz�D{r�D{�D|r�D|�D}r�D}�D~r�D~�Dr�D�D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��D��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD¹HD��HD�9HD�yHDùHD��HD�9HD�yHDĹHD��HD�9HD�yHDŹHD��HD�9HD�yHDƹHD��HD�9HD�yHDǹHD��HD�9HD�yHDȹHD��HD�9HD�yHDɹHD��HD�9HD�yHDʹHD��HD�9HD�yHD˹HD��HD�9HD�yHD̹HD��HD�9HD�yHD͹HD��HD�9HD�yHDιHD��HD�9HD�yHDϹHD��HD�<{D�yHDйHD��HD�9HD�yHDѹHD��HD�9HD�yHDҹHD��HD�9HD�yHDӹHD��HD�9HD�yHDԹHD��HD�9HD�yHDչHD��HD�9HD�yHDֹHD��HD�9HD�yHD׹HD��HD�<{D�yHDعHD��HD�9HD�yHDٹHD��HD�9HD�yHDڹHD��HD�9HD�yHD۹HD��HD�9HD�yHDܹHD��HD�9HD�yHDݹHD��HD�9HD�yHD޹HD��HD�9HD�yHD߹HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�<{D�v11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�AͼjAͶFA͸RAʹ9AͲ-AͲ-AͲ-Aͺ^Aʹ9AͬA�33A�p�A��A�(�A�r�A�Q�A��mA�$�A���A���A�p�A�/A�  A���A�n�A���A���A�9XA��FA��jA�^5A�$�A�z�A�%A���A���A��jA��A�ZA��A�bNA� �A�=qA�33A��A�ffA��;A��DA�bNA��mA��HA�JA���A�l�A�oA��#A�dZA�  A��mA���A��PA�5?A���A��PA���A�1'A��^A�1'A���A�jA�z�A�5?A�|�A���A���A��#A�33A���A�Q�A�A�XA���A�%A���A�ffA���A�1'A��!A�%A�n�A���A���A���A�7LA�A}p�A{�PA{dZAz��Az=qAy��Ay33Ax�DAw��Aw�AvA�Au/At �AsS�AsAr��Ar(�Ao��Am|�AlZAk/Aj1'AiC�Ag
=Aex�AcS�Aa�A`A_�A^ȴA^$�A]�7A\��AZ~�AYXAXI�AW�FAV��AT��AS�ARz�AQoAP1'AO&�AN�AMVALZAKXAI�-AH��AGhsAFr�AE��AE�mAE�;AE�
AE��AEdZACAB��ABA�AA��AA&�A@�9A?��A?XA>�!A=�A;�-A;oA9��A8�A6��A6A�A5S�A4{A2�`A2I�A1A0VA/�A/+A.�\A-C�A,jA+�A*v�A*A)�A)/A(ZA'�-A'\)A&�A&z�A&  A%�A%O�A$�9A#��A"ȴA!��A �9A -AA�;AdZA�A�A-A�mAG�A�+A�A
=A��A��A��A�wA��AA�AJA��A�A{AG�AQ�AJAG�A=qA
�A
$�A	��A	%AbNA��A�TAC�A~�AA�AA"�A��AVA �u@�\)@���@��@���@�~�@�G�@��`@���@�Q�@��@�S�@���@�@�{@�V@�1@�!@�%@�@�-@�hs@��@畁@�K�@��y@�ff@��@�9@�K�@��y@�{@�r�@��H@���@�?}@�j@�S�@��@�  @��@�J@�p�@�9X@Ӆ@�v�@Ѳ-@��@д9@�I�@�t�@�5?@ͺ^@��@�Z@�9X@˕�@�+@ɑh@�b@��@�o@�-@��-@�O�@�1@�M�@�?}@�%@�1'@�C�@�M�@�p�@�|�@�ff@�=q@���@��u@���@��@�v�@�hs@��9@��@���@���@���@�M�@�$�@��h@��`@�+@�M�@��7@�V@���@�A�@��@�C�@�n�@�V@���@�Ĝ@��j@��@�j@��m@�t�@���@�M�@�E�@���@��@�  @��@���@��;@�ƨ@���@�C�@��H@�J@�&�@�r�@��F@��@���@�~�@��#@�G�@���@��9@�  @���@�+@��@�=q@��@���@�Ĝ@�ƨ@�l�@�+@���@�-@��T@�p�@���@�z�@�(�@��w@�o@��R@���@��\@�^5@��T@�O�@��@���@��u@�b@��
@��P@�S�@�"�@��y@���@��\@��+@�v�@�M�@�@��@���@���@��@�hs@��@��/@���@�A�@��;@��w@��F@���@��@�+@���@��H@��!@���@�~�@�ff@�-@�J@���@��7@�p�@�?}@��@���@���@���@�Ĝ@��u@��@�@~�R@~��@~��@~v�@~ff@~V@}�T@}p�@}`B@}O�@|�j@|��@|�D@|z�@|Z@|I�@|Z@{ƨ@{o@z�!@zn�@z^5@zM�@y��@y&�@xr�@w�;@w�@w��@w�@vV@u�@u�-@u�@u�@t��@t�D@tI�@t(�@t1@s��@s�
@s��@sS�@r��@q�@qhs@pĜ@pbN@pb@o�@o�w@o|�@n�R@n��@nV@m�@k�
@j�\@jJ@i�7@h��@h�u@hr�@h1'@h �@hb@h  @h  @g�@g�@g��@g�w@g�@g�@g�@g�@g�@g��@g|�@gl�@gl�@gl�@gl�@g\)@gK�@g
=@f�y@fȴ@f��@fff@e�T@ep�@e`B@e/@d��@d�D@dj@d9X@d1@c��@b�\@bJ@a�^@aX@`��@`�@`Q�@_��@^�@^ff@]�@]`B@\�@\�D@\I�@\(�@[��@[�
@[�F@[��@[dZ@[o@Z^5@Z-@ZJ@Y��@Y��@Yhs@Y&�@Y�@X��@X��@Xb@W��@W��@W|�@W+@Vff@U�@T��@Tj@TI�@TI�@T9X@T1@S�m@S�@R��@R�\@R^5@R-@Q��@Q��@QG�@O�;@N�y@N�R@N��@N{@M/@L�/@L��@Lj@L1@K�F@KdZ@K"�@Jn�@I��@I��@IG�@I�@H��@I%@H��@H�`@H�`@H�@HbN@H �@G�@G��@G�w@G�@G|�@G;d@G+@Fȴ@Fv�@F5?@F@E�h@E/@EV@D��@D�/@D�@DI�@C�m@C��@CdZ@C33@B�H@Bn�@BM�@B=q@BJ@A�^@A�7@Ax�@AX@A�@@�`@@��@@Ĝ@@�u@?�;@?|�@?+@?
=@>�y@>�R@=�@=��@=�h@=�@=p�@=O�@<��@<�/@<�j@<z�@<�@;�
@;t�@:�H@:n�@:^5@:M�@:-@:J@9�7@9X@97L@9%@8��@8�u@8�@8r�@8Q�@7�@7�P@7;d@6�@6�+@5�@5@5`B@4�/@4�D@4z�@4j@3��@3ƨ@3��@3C�@2�H@2n�@2-@2-@2J@1��@1�#@1��@1�^@1��@1�7@1x�@1hs@1G�@0Ĝ@01'@0b@/��@/\)@/�@.�@.�+@.V@.$�@.@-�@-��@-�-@-p�@,�/@,�D@,9X@,1@+�F@+�@+t�@+dZ@+33@*�H@*~�@*-@)�@)��@)G�@)�@(Ĝ@(Ĝ@(�9@(�u@(bN@( �@'�@'l�@'\)@'�@&�+@&V@&$�@%�@%�-@%�@%O�@%?}@%/@$�/@$j@$(�@#�
@#�@#S�@#C�@#33@#33@#o@"�H@"��@"��@"^5@"=q@"�@!��@!�#@!�#@!��@!��@!X@!&�@!&�@!&�@!%@ ��@ �u@ bN@�@��@��@��@�@��@V@�@�-@��@�D@9X@�@dZ@S�@C�@33@"�@o@o@@�H@�!@~�@M�@J@��@�#@X@�`@�9@�u@r�@Q�@b@�w@�@��@l�@+@��@�y@ȴ@�R@��@v�@E�@@@�h@`B@�/@z�@9X@�@��@�F@��@t�@C�@33@o@@@��@�!@~�@M�@-@J@�@�@�#@�^@�7@x�@X@��@Ĝ@�9@��@�u@�u@ �@�@�;@��@��@��@��@�;@�w@\)@+@�@�@�@
=@�@�R@��@v�@E�@@��@�h@�@`B@O�@?}@��@��@�@�D@9X@�m@�F@��@t�@dZ@S�@S�@C�@"�@@
��@
�!@
�\@
~�@
�@	��@	��@	�^@	��@	hs@	G�@	&�@	�@	�@	%@�`@Ĝ@�u@A�@1'@ �@ �@�@�;@�;@�;@�w@�P@l�@K�@+@��@�R@v�@5?@$�@@�T@@��@�@O�@/@/@V@�@�/@�@z�@j@I�@�@�m@ƨ@�@o@�H@��@�!@�!@~�@^5@=q@J11111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�AͼjAͶFA͸RAʹ9AͲ-AͲ-AͲ-Aͺ^G�O�G�O�A�33A�p�A��A�(�A�r�A�Q�A��mA�$�A���A���A�p�A�/A�  A���A�n�A���A���A�9XA��FA��jA�^5A�$�A�z�A�%A���A���A��jA��A�ZA��A�bNA� �A�=qA�33A��A�ffA��;A��DA�bNA��mA��HA�JA���A�l�A�oA��#A�dZA�  A��mA���A��PA�5?A���A��PA���A�1'A��^A�1'A���A�jA�z�A�5?A�|�A���A���A��#A�33A���A�Q�A�A�XA���A�%A���A�ffA���A�1'A��!A�%A�n�A���A���A���A�7LA�A}p�A{�PA{dZAz��Az=qAy��Ay33Ax�DAw��Aw�AvA�Au/At �AsS�AsAr��Ar(�Ao��Am|�AlZAk/Aj1'AiC�Ag
=Aex�AcS�Aa�A`A_�A^ȴA^$�A]�7A\��AZ~�AYXAXI�AW�FAV��AT��AS�ARz�AQoAP1'AO&�AN�AMVALZAKXAI�-AH��AGhsAFr�AE��AE�mAE�;AE�
AE��AEdZACAB��ABA�AA��AA&�A@�9A?��A?XA>�!A=�A;�-A;oA9��A8�A6��A6A�A5S�A4{A2�`A2I�A1A0VA/�A/+A.�\A-C�A,jA+�A*v�A*A)�A)/A(ZA'�-A'\)A&�A&z�A&  A%�A%O�A$�9A#��A"ȴA!��A �9A -AA�;AdZA�A�A-A�mAG�A�+A�A
=A��A��A��A�wA��AA�AJA��A�A{AG�AQ�AJAG�A=qA
�A
$�A	��A	%AbNA��A�TAC�A~�AA�AA"�A��AVA �u@�\)@���@��@���@�~�@�G�@��`@���@�Q�@��@�S�@���@�@�{@�V@�1@�!@�%@�@�-@�hs@��@畁@�K�@��y@�ff@��@�9@�K�@��y@�{@�r�@��H@���@�?}@�j@�S�@��@�  @��@�J@�p�@�9X@Ӆ@�v�@Ѳ-@��@д9@�I�@�t�@�5?@ͺ^@��@�Z@�9X@˕�@�+@ɑh@�b@��@�o@�-@��-@�O�@�1@�M�@�?}@�%@�1'@�C�@�M�@�p�@�|�@�ff@�=q@���@��u@���@��@�v�@�hs@��9@��@���@���@���@�M�@�$�@��h@��`@�+@�M�@��7@�V@���@�A�@��@�C�@�n�@�V@���@�Ĝ@��j@��@�j@��m@�t�@���@�M�@�E�@���@��@�  @��@���@��;@�ƨ@���@�C�@��H@�J@�&�@�r�@��F@��@���@�~�@��#@�G�@���@��9@�  @���@�+@��@�=q@��@���@�Ĝ@�ƨ@�l�@�+@���@�-@��T@�p�@���@�z�@�(�@��w@�o@��R@���@��\@�^5@��T@�O�@��@���@��u@�b@��
@��P@�S�@�"�@��y@���@��\@��+@�v�@�M�@�@��@���@���@��@�hs@��@��/@���@�A�@��;@��w@��F@���@��@�+@���@��H@��!@���@�~�@�ff@�-@�J@���@��7@�p�@�?}@��@���@���@���@�Ĝ@��u@��@�@~�R@~��@~��@~v�@~ff@~V@}�T@}p�@}`B@}O�@|�j@|��@|�D@|z�@|Z@|I�@|Z@{ƨ@{o@z�!@zn�@z^5@zM�@y��@y&�@xr�@w�;@w�@w��@w�@vV@u�@u�-@u�@u�@t��@t�D@tI�@t(�@t1@s��@s�
@s��@sS�@r��@q�@qhs@pĜ@pbN@pb@o�@o�w@o|�@n�R@n��@nV@m�@k�
@j�\@jJ@i�7@h��@h�u@hr�@h1'@h �@hb@h  @h  @g�@g�@g��@g�w@g�@g�@g�@g�@g�@g��@g|�@gl�@gl�@gl�@gl�@g\)@gK�@g
=@f�y@fȴ@f��@fff@e�T@ep�@e`B@e/@d��@d�D@dj@d9X@d1@c��@b�\@bJ@a�^@aX@`��@`�@`Q�@_��@^�@^ff@]�@]`B@\�@\�D@\I�@\(�@[��@[�
@[�F@[��@[dZ@[o@Z^5@Z-@ZJ@Y��@Y��@Yhs@Y&�@Y�@X��@X��@Xb@W��@W��@W|�@W+@Vff@U�@T��@Tj@TI�@TI�@T9X@T1@S�m@S�@R��@R�\@R^5@R-@Q��@Q��@QG�@O�;@N�y@N�R@N��@N{@M/@L�/@L��@Lj@L1@K�F@KdZ@K"�@Jn�@I��@I��@IG�@I�@H��@I%@H��@H�`@H�`@H�@HbN@H �@G�@G��@G�w@G�@G|�@G;d@G+@Fȴ@Fv�@F5?@F@E�h@E/@EV@D��@D�/@D�@DI�@C�m@C��@CdZ@C33@B�H@Bn�@BM�@B=q@BJ@A�^@A�7@Ax�@AX@A�@@�`@@��@@Ĝ@@�u@?�;@?|�@?+@?
=@>�y@>�R@=�@=��@=�h@=�@=p�@=O�@<��@<�/@<�j@<z�@<�@;�
@;t�@:�H@:n�@:^5@:M�@:-@:J@9�7@9X@97L@9%@8��@8�u@8�@8r�@8Q�@7�@7�P@7;d@6�@6�+@5�@5@5`B@4�/@4�D@4z�@4j@3��@3ƨ@3��@3C�@2�H@2n�@2-@2-@2J@1��@1�#@1��@1�^@1��@1�7@1x�@1hs@1G�@0Ĝ@01'@0b@/��@/\)@/�@.�@.�+@.V@.$�@.@-�@-��@-�-@-p�@,�/@,�D@,9X@,1@+�F@+�@+t�@+dZ@+33@*�H@*~�@*-@)�@)��@)G�@)�@(Ĝ@(Ĝ@(�9@(�u@(bN@( �@'�@'l�@'\)@'�@&�+@&V@&$�@%�@%�-@%�@%O�@%?}@%/@$�/@$j@$(�@#�
@#�@#S�@#C�@#33@#33@#o@"�H@"��@"��@"^5@"=q@"�@!��@!�#@!�#@!��@!��@!X@!&�@!&�@!&�@!%@ ��@ �u@ bN@�@��@��@��@�@��@V@�@�-@��@�D@9X@�@dZ@S�@C�@33@"�@o@o@@�H@�!@~�@M�@J@��@�#@X@�`@�9@�u@r�@Q�@b@�w@�@��@l�@+@��@�y@ȴ@�R@��@v�@E�@@@�h@`B@�/@z�@9X@�@��@�F@��@t�@C�@33@o@@@��@�!@~�@M�@-@J@�@�@�#@�^@�7@x�@X@��@Ĝ@�9@��@�u@�u@ �@�@�;@��@��@��@��@�;@�w@\)@+@�@�@�@
=@�@�R@��@v�@E�@@��@�h@�@`B@O�@?}@��@��@�@�D@9X@�m@�F@��@t�@dZ@S�@S�@C�@"�@@
��@
�!@
�\@
~�@
�@	��@	��@	�^@	��@	hs@	G�@	&�@	�@	�@	%@�`@Ĝ@�u@A�@1'@ �@ �@�@�;@�;@�;@�w@�P@l�@K�@+@��@�R@v�@5?@$�@@�T@@��@�@O�@/@/@V@�@�/@�@z�@j@I�@�@�m@ƨ@�@o@�H@��@�!@�!@~�@^5@=q@J11111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B�B�XB�Bt�B�jB��B�hB��B�{B}�Bx�B�B�oB��B��B��B��B��B��B��B�RB��B��B�'B�oBo�BR�BP�BZB>wB(�B1BB\B��B��BBJB	7BuB��BBB
=B+BBB��B�B��B�!B��B�wB�XB�FB��B��B��B�hB�JB~�BffBO�BN�BB�B/B�B
��BVBB
��B
�B
�NB
�/B
��B
��B
�RB
��B
��B
��B
�{B
�JB
�+B
v�B
m�B
o�B
YB
M�B
gmB
aHB
XB
XB
Q�B
L�B
A�B
C�B
7LB
/B
,B
%�B
)�B
"�B
oB	�B	�yB	�B	�`B	�HB	��B	�wB	�9B	��B	��B	��B	��B	��B	�uB	�DB	�B	jB	p�B	k�B	o�B	dZB	Q�B	M�B	O�B	D�B	A�B	>wB	<jB	,B	1'B	$�B	�B	�B	bB	oB	�B	�B	�B	�B	�B	PB��B��B	B��B��B��B�B�B�yB�)B��B�B��BBBƨB�dB�-B�9B�9B�-B��B��B�B��B��B��B��B�bB��B��B�hB�PB�DB�bB�VB�DB�7B�1B�B� Bt�Bs�Br�Bp�Bv�Bl�BgmBt�Bt�Bp�Bq�Bp�BiyBbNBcTBYB\)BR�BR�BK�BJ�BW
BXBO�BE�BB�B=qB1'B"�B7LB49B'�B;dB8RB2-B0!B�B,B1'B-B33B,B$�B�B �B'�B�B!�B%�B!�B"�B%�B-B-B,B&�B �B�BoB�B�B�B�B�B\B'�B(�B"�B%�B,B(�B'�B%�B �B�B(�B#�B�B�B$�B'�B%�B!�B�B�B$�B&�B'�B%�B'�B'�B)�B+B-B,B'�B&�B,B,B,B0!B)�B&�B�B�BhB"�B.B33B1'B)�B'�B1'B9XB49B2-B33B33B/B9XBD�BA�B;dBA�BA�BF�B@�BD�BB�BE�BA�B?}BH�BZBVBS�BN�B\)B`BBffBiyBgmBhsBiyBiyBhsBy�B|�B|�Bz�Bx�Bv�By�Bw�B� B�B{�B}�B�B�VB�\B�PB�PB�JB�=B�PB�=B�PB�uB��B��B��B��B��B��B��B��B��B��B�B�-B�9B�^B�^B�?B�LBĜBǮBƨBǮB��B��B��B��B�
B�B�5B�`B�B�B�B�yB�B�B�B�B�B��B��B��B��B��B��B	B	%B	%B	%B	%B	
=B		7B		7B	DB	JB	DB	PB	\B	bB	{B	�B	�B	�B	�B	�B	�B	"�B	#�B	&�B	&�B	'�B	&�B	(�B	(�B	,B	0!B	33B	5?B	8RB	<jB	;dB	9XB	:^B	;dB	>wB	E�B	H�B	H�B	G�B	H�B	H�B	I�B	M�B	R�B	S�B	VB	ZB	\)B	\)B	\)B	^5B	]/B	\)B	_;B	e`B	gmB	hsB	hsB	ffB	ffB	jB	o�B	s�B	t�B	r�B	s�B	v�B	x�B	y�B	z�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�DB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�9B	�FB	�LB	�XB	�XB	�^B	�^B	�^B	�^B	�^B	�dB	�jB	�qB	�qB	�qB	�jB	�jB	�qB	�qB	�wB	�wB	�wB	�wB	�qB	�qB	�}B	��B	��B	�}B	��B	B	ƨB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	ɺB	��B	��B	��B	��B	��B	�B	�
B	�
B	��B	�B	�B	�#B	�)B	�BB	�BB	�HB	�HB	�TB	�ZB	�ZB	�ZB	�TB	�NB	�mB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�yB	�B	�B	�B	�B	�yB	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B
  B
  B	��B	��B
B
B
B
B
B
B
B
B
+B
1B
	7B
DB
JB
PB
JB
JB
JB
DB
JB
JB
PB
VB
VB
VB
PB
PB
VB
PB
PB
VB
VB
VB
\B
oB
oB
hB
hB
bB
hB
uB
uB
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
"�B
"�B
!�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
%�B
$�B
$�B
$�B
%�B
%�B
%�B
(�B
'�B
(�B
)�B
-B
,B
+B
,B
,B
,B
,B
-B
/B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
0!B
0!B
.B
.B
2-B
1'B
2-B
33B
33B
33B
49B
5?B
5?B
6FB
5?B
5?B
49B
33B
5?B
5?B
6FB
7LB
8RB
8RB
8RB
7LB
6FB
7LB
8RB
9XB
9XB
:^B
;dB
;dB
=qB
=qB
<jB
<jB
<jB
<jB
=qB
>wB
=qB
<jB
>wB
?}B
?}B
?}B
@�B
@�B
A�B
@�B
?}B
?}B
@�B
A�B
A�B
C�B
C�B
D�B
D�B
D�B
C�B
D�B
D�B
C�B
D�B
E�B
E�B
E�B
F�B
E�B
E�B
E�B
E�B
G�B
G�B
F�B
F�B
F�B
F�B
E�B
H�B
H�B
F�B
E�B
F�B
G�B
G�B
H�B
F�B
J�B
J�B
I�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
N�B
N�B
N�B
N�B
N�B
O�B
N�B
M�B
N�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
R�B
R�B
S�B
T�B
T�B
T�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
S�B
T�B
W
B
XB
XB
XB
YB
YB
YB
ZB
ZB
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
\)B
[#B
]/B
^5B
^5B
^5B
^5B
]/B
^5B
`BB
`BB
`BB
`BB
`BB
`BB
_;B
^5B
_;B
aHB
aHB
aHB
`BB
_;B
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
cTB
cTB
bNB
bNB
cTB
bNB
bNB
cTB
dZB
e`B
ffB
ffB
ffB
ffB
gmB
ffB
ffB
ffB
ffB
gmB
gmB
ffB
hsB
hsB
iyB
iyB
hsB
iyB
iyB
jB
jB
iyB
iyB
iyB
iyB
iyB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
k�B
l�B
l�B
k�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
p�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
o�B
p�B
p�B
o�B
q�B
r�B
s�B
s�B
r�B
s�B
s�B
s�B
t�11111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B�B�B��G�O�G�O�B��By�B��B��B�B��B�B��Bz�B�AB��B�
B�+B��B� B��B�]B�B�*B͟BϑB�3B��BsBW
BS[B[�B@�B,B0B�B�B�B��BmB�B
XBaB iBGB�B
�B�BB�B�2B�B� B��B�=B�.B�DB�2B��B�~B��B��B�6B��Bh�BR�BP�BD�B1�B5B �BBBaB
�$B
��B
�&B
ބB
�B
��B
��B
��B
�$B
�dB
��B
�jB
��B
x�B
o�B
p�B
[�B
O�B
g�B
a�B
X�B
X�B
R�B
M�B
B�B
DgB
8�B
0oB
-CB
&�B
*�B
#nB
�B	��B	��B	�B	�B	�B	өB	�;B	�zB	�zB	��B	��B	��B	��B	�{B	�dB	��B	m)B	rB	mB	p�B	fB	TaB	O�B	QNB	FYB	B�B	?�B	=�B	-�B	2GB	&�B	�B	B	�B	�B	$B	�B	�B	�B	�B	VB��B�VB	�B��B��B��B��B�B��B�OBҽB�=BοBĶB�B��B��B�B��B�?B�MB��B�B��B�
B�B��B��B��B�QB�1B�oB��B�JB� B�B�B�	B��B��B��BvFBu?BtBrGBw�Bn/Bh�BuZBuZBq�Br-BqABjBc�Bd@BZ�B\�BT{BTFBMPBLJBW�BX�BP�BGBC�B>�B2�B%�B8�B5�B)�B<PB9XB3MB1[B�B-)B2-B.IB3�B-B&BqB!�B(�B B"�B&�B"�B#�B&�B-wB-]B,qB'�B!�B�B�B�B�B�B�B�BB($B)yB#�B&fB,qB)yB(�B&�B!�B �B)yB$�B B�B%�B(�B&�B"�B�B/B%�B'�B(�B&�B(�B(�B*�B+�B-�B,�B(�B'�B,�B,�B,�B0�B*�B'�BBB�B$B.�B3�B1�B+6B)_B1�B9�B5B2�B4B4B0�B:*BD�BBAB<PBB[BB[BG+BAoBESBC{BFYBB�B@�BI�BZQBV�BT�BP.B\�B`�Bf�Bi�Bh
BiBjBjKBiyBzB}"B}<B{0ByXBwfBz^Bx�B�iB�[B|�B~�B��B�pB�vB��B��B��B��B��B�B�"B�,B�_B�OB�@B�FB�|B�TB�ZB�XB��B�eB��B��B��B��B��B�B�B��B��B�+B�1B�)B�PB�\B�uB�sBٚB��B��B�B�B��B�B�B�B�B�B�MB�B�$B�*B�6B�<B�HB	MB	YB	YB	tB	tB	
rB		�B		�B	�B	~B	�B	�B	�B	�B	�B	�B	�B	�B	�B		B	B	#B	$&B	'B	'B	($B	'RB	)_B	)_B	,qB	0UB	3�B	5tB	8�B	<�B	;�B	9�B	:�B	<B	>�B	E�B	H�B	H�B	G�B	H�B	H�B	J#B	N"B	S&B	T,B	VSB	ZQB	\]B	\xB	\]B	^OB	]~B	\xB	_�B	e�B	g�B	h�B	h�B	f�B	f�B	j�B	o�B	s�B	t�B	sB	tB	wB	y	B	zB	{0B	}"B	~(B	�OB	�AB	�GB	�gB	�MB	�gB	�YB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�QB	�OB	�nB	�kB	�iB	�oB	�nB	�zB	��B	�rB	��B	��B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�DB	�.B	�@B	�[B	�FB	�SB	�?B	�sB	ՁB	�eB	�kB	�qB	�xB	�\B	��B	�|B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�	B	�	B	�	B	�B	�B	�B	�XB
 4B
 4B	�]B	�VB
AB
MB
gB
aB
MB
SB
mB
�B
zB
fB
	lB
xB
~B
jB
dB
~B
dB
�B
~B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
B
�B
B
B
!B
#�B
#�B
#B
# B
"B
$B
%B
%B
%B
&B
'B
'B
&B
%,B
%FB
%,B
&B
&2B
&LB
)*B
(>B
)DB
*0B
-)B
,=B
+6B
,=B
,=B
,=B
,qB
-wB
/OB
0;B
0UB
0UB
0oB
1AB
1AB
1AB
1[B
1AB
0oB
0UB
.cB
.cB
2aB
1vB
2aB
3hB
3hB
3hB
4nB
5�B
5tB
6zB
5tB
5tB
4nB
3hB
5�B
5tB
6zB
7�B
8�B
8lB
8�B
7�B
6�B
7�B
8�B
9�B
9�B
:�B
;�B
;�B
=�B
=�B
<�B
<�B
<�B
<�B
=�B
>�B
=�B
<�B
>�B
?�B
?�B
?�B
@�B
@�B
A�B
@�B
?�B
?�B
@�B
A�B
A�B
C�B
C�B
D�B
D�B
D�B
C�B
D�B
D�B
C�B
D�B
E�B
E�B
E�B
F�B
E�B
E�B
E�B
E�B
G�B
G�B
F�B
F�B
F�B
F�B
E�B
H�B
H�B
F�B
E�B
F�B
G�B
G�B
IB
GB
J�B
J�B
J#B
OB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
OB
OB
OB
OB
OB
PB
OB
N"B
O(B
R:B
R:B
R B
R B
R B
R B
SB
TB
S&B
S&B
TB
UB
U2B
UB
T,B
T,B
T,B
T,B
U2B
UMB
UMB
TFB
UMB
W?B
XEB
X_B
XEB
Y1B
YKB
YKB
Z7B
ZQB
ZQB
Z7B
ZQB
ZQB
ZQB
ZQB
[WB
\CB
\]B
\]B
\CB
\]B
\]B
\]B
\]B
[qB
]dB
^OB
^OB
^OB
^OB
]�B
^jB
`\B
`\B
`\B
`\B
`\B
`vB
_pB
^�B
_pB
abB
a|B
abB
`vB
_pB
`vB
`vB
`vB
`vB
`vB
a|B
a|B
bhB
b�B
cnB
cnB
b�B
b�B
cnB
b�B
b�B
cnB
d�B
e�B
f�B
f�B
f�B
f�B
g�B
f�B
f�B
f�B
f�B
g�B
g�B
f�B
h�B
h�B
i�B
i�B
h�B
i�B
i�B
j�B
j�B
i�B
i�B
i�B
i�B
i�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
k�B
k�B
l�B
l�B
k�B
l�B
l�B
k�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
p�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
o�B
p�B
p�B
o�B
q�B
r�B
s�B
s�B
r�B
s�B
s�B
s�B
t�11111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<K+<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.21(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807170033372018071700333720180717003337201807170200162018071702001620180717020016201807180021152018071800211520180718002115  JA  ARFMdecpA19c                                                                20180723123517  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180723033627  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180723033630  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180723033631  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180723033631  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180723033631  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180723033631  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180723033631  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180723033631  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180723033631  QCF$                G�O�G�O�G�O�            4000JA      jafc1.0                                                                 20180723033632                      G�O�G�O�G�O�                JA  ARUP                                                                        20180723040120                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180713153458  CV  JULD            G�O�G�O�FË�                JM  ARCAJMQC2.0                                                                 20180716153337  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180716153337  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180716170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180717152115  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                