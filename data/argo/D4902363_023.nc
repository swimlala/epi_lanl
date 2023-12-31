CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-04T00:35:25Z creation;2016-08-04T00:35:29Z conversion to V3.1;2019-12-19T08:33:56Z update;     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160804003525  20200115101518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_023                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��u�7�1   @��v�W @;�/��w�dl�t�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @\)@�G�@�G�A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B�\BB'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[��C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�Dur�Du�Dvr�Dv�Dwr�Dw�Dxr�Dx�Dyr�Dy�Dzr�Dz�D{r�D{�D|r�D|�D}r�D}�D~r�D~�Dr�D�D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��D��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��D�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�yHD¹HD��HD�9HD�yHDùHD��HD�9HD�yHDĹHD��HD�9HD�yHDŹHD��HD�9HD�yHDƹHD��HD�9HD�yHDǹHD��HD�9HD�yHDȹHD��HD�9HD�yHDɹHD��HD�9HD�yHDʹHD��HD�9HD�yHD˹HD��HD�9HD�yHD̹HD��HD�9HD�yHD͹HD��HD�9HD�yHDιHD��HD�9HD�yHDϹHD��HD�9HD�yHDйHD��HD�9HD�yHDѹHD��HD�9HD�yHDҹHD��HD�9HD�yHDӹHD��HD�9HD�yHDԹHD��HD�9HD�yHDչHD��HD�9HD�yHDֹHD��HD�9HD�yHD׹HD��HD�9HD�yHDعHD��HD�9HD�yHDٹHD��HD�9HD�yHDڹHD��HD�9HD�yHD۹HD��HD�9HD�yHDܹHD��HD�9HD�yHDݹHD��HD�9HD�yHD޹HD��HD�9HD�yHD߹HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD�HD��HD�9HD�yHD��HD��HD�9HD�yHD��HD��HD�9HD�|{D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�ȴA�ĜA���A;wA���A;wAʹ9Aʹ9AͲ-AͬA͝�A̓A�l�A�dZA�\)A�XA��A�33A�/A��A��A��-A�1A�
=A�C�A��hA�$�A���A��A��A�O�A���A�O�A�ƨA��yA�oA�"�A��+A��^A��A�&�A��A��`A�ffA��yA��+A��7A���A�Q�A�hsA��A��+A�~�A��;A��`A�^5A���A��
A�A�VA�ĜA�jA��PA�  A��A�dZA�"�A��A��PA�\)A���A�JA�VA��FA�I�A��/A�v�A�  A�r�A��-A�7LA�7A~$�A}S�A|v�A{�^A{C�Azr�Ay�;Ax�jAxz�Aw��Av��Au�-At�AsƨAr�!Aqp�Ao��An��AmG�Al�RAlI�Ak�-Aj��AiAg��Af�uAe�hAdn�Ac&�Aa�A`�9A`E�A`bA_�TA^�9A\��A[�AYƨAW�AV�AV9XAUp�AT�`AT5?AS�ASG�AQ��AQC�APjAO�wAOANM�AM�^AM/ALbAJI�AG�
AG�AF�AFjAE�AC�7AA|�A@bNA?�A>$�A=�
A=��A=O�A<�\A< �A;�-A;��A;�A;G�A;?}A:�HA:�uA:bA9
=A81'A7VA6JA5C�A4�\A3�A2�\A25?A1�A1��A0�`A0JA.��A-�-A-S�A,�A,1A+�-A+l�A+G�A*��A*M�A*  A)�A)/A(�A'/A&�RA%A%/A$��A#�#A"JA �/A {A\)A��A��A��A5?A��AE�A�#A+A�A1A+Ar�A�A�;A�wAx�A�`A��Ar�AVA\)A��AZA�PAA�A�FA��Al�A	�#A	`BA	7LA	�A��A�9A��A�A��Al�A�HAz�AXAM�At�A V@�;d@�ȴ@���@���@���@�5?@�@�p�@�V@��`@���@��@�D@�R@�"�@�&�@�F@�V@�`B@��`@�ƨ@���@���@���@�@ߍP@�hs@�C�@�v�@��@�%@�z�@և+@�?}@ԋD@���@�33@�n�@��@ёh@�?}@�A�@�/@˝�@�5?@�7L@Ǯ@��@�M�@�p�@�t�@��@���@�@�v�@��^@�&�@��u@�K�@��H@���@�E�@��@�&�@�I�@���@�%@��;@��y@���@��/@�z�@�1@���@���@���@�%@��9@�j@��m@�"�@�v�@�{@��^@�G�@�Z@��;@���@�dZ@�-@�V@�(�@��@�\)@��y@��@�x�@�Ĝ@�Z@�Z@�Z@�I�@� �@�ƨ@�"�@�n�@���@�{@���@�p�@�7L@��@�b@�1'@�b@��@�\)@���@�E�@��@���@��@�X@�&�@��@�S�@�l�@�dZ@�dZ@�33@��@�ȴ@���@��+@�J@��h@�?}@��`@��@��R@�=q@�J@��@��@�/@�x�@��@�X@�%@��m@��H@�$�@�J@��h@�j@��@�ff@�@��@�dZ@�$�@��@��@��@��T@�@���@�&�@�&�@�?}@�7L@�/@�`B@�`B@�/@���@��9@��@�9X@��
@��@�|�@�t�@�\)@�K�@�;d@�\)@��w@�ƨ@�K�@���@���@�ȴ@�ȴ@��R@�~�@��@�J@�@�hs@�A�@�$�@�?}@���@���@��/@��@��D@�z�@�A�@�b@�@\)@~v�@}�@}�@}O�@|I�@|�@|(�@}V@}�@~5?@{��@y�^@x�`@xbN@x�`@yhs@z=q@z�H@z�@z��@y��@x�9@x��@x��@x�@x  @xbN@xA�@w|�@v��@u�T@u�@u/@tZ@s�F@s�F@s�@sdZ@sS�@sS�@sS�@sC�@sC�@s�m@t�j@tZ@r�@r�@q��@r�@rn�@r~�@r�!@t9X@t�/@u�@u�@tI�@s�m@s�
@s�F@s��@so@r^5@r^5@r^5@r-@qx�@pr�@pb@o\)@m@m?}@m�h@m�@m��@m/@l��@k��@j��@i��@i�@i7L@ix�@jM�@iX@i%@h�9@hbN@h�@i�7@i��@iX@ihs@h1'@e�-@d(�@c��@c��@c��@cƨ@d�@d1@c�m@cƨ@c��@ct�@c@b~�@b-@a�@a��@a��@a��@a�7@ax�@a7L@`�u@_�@^��@^{@]@]/@]V@]�@\�j@\I�@\�@[ƨ@[��@[S�@["�@[@[33@Z�!@Y�#@Y�^@Y�^@Y7L@X��@X�u@Xb@W��@Wl�@W;d@W
=@V�R@V��@V�+@U�@U��@UO�@UV@T�@T�j@T�@TZ@S�F@SdZ@S@R�!@R~�@R=q@RJ@Q��@Q7L@Q�@Q�@P��@P��@PbN@P1'@O�P@O\)@OK�@O;d@O+@O
=@N�y@N��@Nff@N5?@N$�@N{@M�T@Mp�@L��@L�@L�D@L(�@K�
@Kƨ@K��@Kt�@Ko@J�!@J~�@J-@I��@Ihs@I7L@I%@H��@Hr�@Hb@H  @G�;@G��@G�w@G|�@G�@F��@F��@FE�@E�T@E@E��@E�@E?}@EV@D�@Dz�@D1@C"�@B�!@B~�@B-@A�#@Ahs@AG�@AG�@A7L@@��@@�9@@�@@A�@?��@>ȴ@>$�@>{@>{@=�T@=�h@=p�@=`B@=�@<��@<(�@;��@;t�@;C�@;o@:��@:~�@:M�@:=q@:=q@:-@:J@9�^@9x�@97L@97L@9&�@8��@8�u@8Q�@8A�@8A�@81'@8 �@7�;@7��@7l�@7+@6�@6�+@6ff@6ff@65?@6{@5��@5�-@5�@5`B@5?}@5/@5/@5V@4��@4��@4�@4��@4�D@4Z@4�@3��@3�F@3dZ@333@2��@2~�@2~�@2n�@2J@1��@1x�@17L@1%@0Ĝ@0Q�@/��@/��@/\)@/+@/�@/
=@.��@.��@.$�@-�T@-@-�h@,��@,�D@+�
@+dZ@+C�@+33@+"�@*��@*��@*M�@*�@*J@)��@)��@)�@)�#@)��@)�7@)X@(Ĝ@(b@'�@'��@'��@'+@&��@&V@&5?@&{@%�T@%��@%��@%V@$�@$z�@$j@$j@$z�@$z�@$Z@$(�@#ƨ@#�@#33@#@"�H@"��@"�\@"M�@"J@!��@!��@!��@!��@!�7@!hs@!&�@!%@!%@ Ĝ@ �u@ �u@ bN@  �@ b@�@�w@�@��@l�@\)@K�@
=@�@��@v�@5?@�@V@��@I�@1@�m@�@S�@S�@33@o@o@o@o@@@�@�@��@��@��@�\@n�@M�@�@�^@��@7L@��@�@bN@Q�@Q�@b@|�@K�@ȴ@V@{@@�@p�@�@�j@z�@I�@9X@(�@1@�F@C�@"�@@��@n�@M�@M�@M�@�@x�@�@Ĝ@�9@�u@�@r�@A�@ �@�;@�@l�@K�@;d@+@+@
=@��@�y@�y@�@ȴ@��@��@5?@{@$�@�@��@�-@?}@�j@�D@z�@j@j@Z@I�@I�@9X@9X@9X@9X@(�@��@t�@C�@33@"�@
�H@
��@
��@
��@
�\@
~�@
~�@
~�@
n�@
M�@
�@
J@	��@
J@	��@	��@	��@	�@	�@	�@	�#@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�ȴA�ĜA���A;wA���A;wAʹ9Aʹ9AͲ-AͬA͝�A̓A�l�A�dZA�\)A�XA��A�33A�/A��A��A��-A�1A�
=A�C�A��hA�$�A���A��A��A�O�A���A�O�A�ƨA��yA�oA�"�A��+A��^A��A�&�A��A��`A�ffA��yA��+A��7A���A�Q�A�hsA��A��+A�~�A��;A��`A�^5A���A��
A�A�VA�ĜA�jA��PA�  A��A�dZA�"�A��A��PA�\)A���A�JA�VA��FA�I�A��/A�v�A�  A�r�A��-A�7LA�7A~$�A}S�A|v�A{�^A{C�Azr�Ay�;Ax�jAxz�Aw��Av��Au�-At�AsƨAr�!Aqp�Ao��An��AmG�Al�RAlI�Ak�-Aj��AiAg��Af�uAe�hAdn�Ac&�Aa�A`�9A`E�A`bA_�TA^�9A\��A[�AYƨAW�AV�AV9XAUp�AT�`AT5?AS�ASG�AQ��AQC�APjAO�wAOANM�AM�^AM/ALbAJI�AG�
AG�AF�AFjAE�AC�7AA|�A@bNA?�A>$�A=�
A=��A=O�A<�\A< �A;�-A;��A;�A;G�A;?}A:�HA:�uA:bA9
=A81'A7VA6JA5C�A4�\A3�A2�\A25?A1�A1��A0�`A0JA.��A-�-A-S�A,�A,1A+�-A+l�A+G�A*��A*M�A*  A)�A)/A(�A'/A&�RA%A%/A$��A#�#A"JA �/A {A\)A��A��A��A5?A��AE�A�#A+A�A1A+Ar�A�A�;A�wAx�A�`A��Ar�AVA\)A��AZA�PAA�A�FA��Al�A	�#A	`BA	7LA	�A��A�9A��A�A��Al�A�HAz�AXAM�At�A V@�;d@�ȴ@���@���@���@�5?@�@�p�@�V@��`@���@��@�D@�R@�"�@�&�@�F@�V@�`B@��`@�ƨ@���@���@���@�@ߍP@�hs@�C�@�v�@��@�%@�z�@և+@�?}@ԋD@���@�33@�n�@��@ёh@�?}@�A�@�/@˝�@�5?@�7L@Ǯ@��@�M�@�p�@�t�@��@���@�@�v�@��^@�&�@��u@�K�@��H@���@�E�@��@�&�@�I�@���@�%@��;@��y@���@��/@�z�@�1@���@���@���@�%@��9@�j@��m@�"�@�v�@�{@��^@�G�@�Z@��;@���@�dZ@�-@�V@�(�@��@�\)@��y@��@�x�@�Ĝ@�Z@�Z@�Z@�I�@� �@�ƨ@�"�@�n�@���@�{@���@�p�@�7L@��@�b@�1'@�b@��@�\)@���@�E�@��@���@��@�X@�&�@��@�S�@�l�@�dZ@�dZ@�33@��@�ȴ@���@��+@�J@��h@�?}@��`@��@��R@�=q@�J@��@��@�/@�x�@��@�X@�%@��m@��H@�$�@�J@��h@�j@��@�ff@�@��@�dZ@�$�@��@��@��@��T@�@���@�&�@�&�@�?}@�7L@�/@�`B@�`B@�/@���@��9@��@�9X@��
@��@�|�@�t�@�\)@�K�@�;d@�\)@��w@�ƨ@�K�@���@���@�ȴ@�ȴ@��R@�~�@��@�J@�@�hs@�A�@�$�@�?}@���@���@��/@��@��D@�z�@�A�@�b@�@\)@~v�@}�@}�@}O�@|I�@|�@|(�@}V@}�@~5?@{��@y�^@x�`@xbN@x�`@yhs@z=q@z�H@z�@z��@y��@x�9@x��@x��@x�@x  @xbN@xA�@w|�@v��@u�T@u�@u/@tZ@s�F@s�F@s�@sdZ@sS�@sS�@sS�@sC�@sC�@s�m@t�j@tZ@r�@r�@q��@r�@rn�@r~�@r�!@t9X@t�/@u�@u�@tI�@s�m@s�
@s�F@s��@so@r^5@r^5@r^5@r-@qx�@pr�@pb@o\)@m@m?}@m�h@m�@m��@m/@l��@k��@j��@i��@i�@i7L@ix�@jM�@iX@i%@h�9@hbN@h�@i�7@i��@iX@ihs@h1'@e�-@d(�@c��@c��@c��@cƨ@d�@d1@c�m@cƨ@c��@ct�@c@b~�@b-@a�@a��@a��@a��@a�7@ax�@a7L@`�u@_�@^��@^{@]@]/@]V@]�@\�j@\I�@\�@[ƨ@[��@[S�@["�@[@[33@Z�!@Y�#@Y�^@Y�^@Y7L@X��@X�u@Xb@W��@Wl�@W;d@W
=@V�R@V��@V�+@U�@U��@UO�@UV@T�@T�j@T�@TZ@S�F@SdZ@S@R�!@R~�@R=q@RJ@Q��@Q7L@Q�@Q�@P��@P��@PbN@P1'@O�P@O\)@OK�@O;d@O+@O
=@N�y@N��@Nff@N5?@N$�@N{@M�T@Mp�@L��@L�@L�D@L(�@K�
@Kƨ@K��@Kt�@Ko@J�!@J~�@J-@I��@Ihs@I7L@I%@H��@Hr�@Hb@H  @G�;@G��@G�w@G|�@G�@F��@F��@FE�@E�T@E@E��@E�@E?}@EV@D�@Dz�@D1@C"�@B�!@B~�@B-@A�#@Ahs@AG�@AG�@A7L@@��@@�9@@�@@A�@?��@>ȴ@>$�@>{@>{@=�T@=�h@=p�@=`B@=�@<��@<(�@;��@;t�@;C�@;o@:��@:~�@:M�@:=q@:=q@:-@:J@9�^@9x�@97L@97L@9&�@8��@8�u@8Q�@8A�@8A�@81'@8 �@7�;@7��@7l�@7+@6�@6�+@6ff@6ff@65?@6{@5��@5�-@5�@5`B@5?}@5/@5/@5V@4��@4��@4�@4��@4�D@4Z@4�@3��@3�F@3dZ@333@2��@2~�@2~�@2n�@2J@1��@1x�@17L@1%@0Ĝ@0Q�@/��@/��@/\)@/+@/�@/
=@.��@.��@.$�@-�T@-@-�h@,��@,�D@+�
@+dZ@+C�@+33@+"�@*��@*��@*M�@*�@*J@)��@)��@)�@)�#@)��@)�7@)X@(Ĝ@(b@'�@'��@'��@'+@&��@&V@&5?@&{@%�T@%��@%��@%V@$�@$z�@$j@$j@$z�@$z�@$Z@$(�@#ƨ@#�@#33@#@"�H@"��@"�\@"M�@"J@!��@!��@!��@!��@!�7@!hs@!&�@!%@!%@ Ĝ@ �u@ �u@ bN@  �@ b@�@�w@�@��@l�@\)@K�@
=@�@��@v�@5?@�@V@��@I�@1@�m@�@S�@S�@33@o@o@o@o@@@�@�@��@��@��@�\@n�@M�@�@�^@��@7L@��@�@bN@Q�@Q�@b@|�@K�@ȴ@V@{@@�@p�@�@�j@z�@I�@9X@(�@1@�F@C�@"�@@��@n�@M�@M�@M�@�@x�@�@Ĝ@�9@�u@�@r�@A�@ �@�;@�@l�@K�@;d@+@+@
=@��@�y@�y@�@ȴ@��@��@5?@{@$�@�@��@�-@?}@�j@�D@z�@j@j@Z@I�@I�@9X@9X@9X@9X@(�@��@t�@C�@33@"�@
�H@
��@
��@
��@
�\@
~�@
~�@
~�@
n�@
M�@
�@
J@	��@
J@	��@	��@	��@	�@	�@	�@	�#@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B8RB8RB8RB8RB8RB8RB9XB:^B;dB<jB=qB@�BC�BF�BE�BE�BE�BF�BF�BhsBdZBXB?}B&�B+B�BǮB�XB�-B�'B�'B��B��B�hB�JB�%Bp�BXBG�B/B#�B	7B��B�B�B�HB��B�RB��B��B��B�\B�=B�1B�Bu�BcTB\)BYBW
BR�BJ�BE�B<jB49B0!B+B'�B#�B�B�B�B1B
��B
�B
�B
�TB
�5B
��B
��B
��B
�LB
�-B
��B
��B
��B
�oB
�\B
�1B
�B
z�B
w�B
r�B
jB
cTB
ZB
R�B
H�B
@�B
2-B
+B
 �B
�B
�B
{B
bB
B	��B	�B	�B	�sB	�BB	�B	��B	��B	ȴB	ǮB	�}B	�9B	�B	��B	�7B	�B	}�B	z�B	{�B	y�B	w�B	t�B	k�B	gmB	cTB	`BB	]/B	ZB	XB	S�B	L�B	A�B	49B	.B	,B	(�B	#�B	�B	{B	VB	1B	B	  B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�`B�HB�;B�/B�B��B��B��B��B��BƨBB�qB�dB�XB�FB�9B�3B�-B�'B�B�B�B��B��B��B��B��B��B��B��B�VB�=B�+B�B�B� B|�B{�Bz�Bx�Bw�Bt�Br�Bq�Bp�Bm�Bm�Bk�Bk�Bk�BiyBhsBffBgmBaHB_;B]/B]/BZBW
BVBP�BL�BK�BJ�BJ�BJ�BI�BJ�BI�BF�BD�BD�BF�BH�BE�BC�B@�B:^B8RB7LB6FB2-B0!B0!B0!B2-B49B:^B;dB9XB6FB49B0!B-B'�B%�B'�B%�B(�B+B+B'�B&�B(�B&�B&�B+B(�B)�B(�B'�B'�B'�B&�B'�B&�B&�B&�B&�B(�B(�B(�B(�B+B+B+B,B/B.B.B.B.B/B0!B1'B33B49B49B6FB5?B7LB9XB<jB>wB@�BA�BC�BC�BD�BD�BE�BG�BI�BK�BL�BM�BN�BP�BQ�BR�BR�BS�BVBW
BW
BW
B[#B^5BaHBaHBbNBbNBdZBhsBk�Bk�Bk�BjBk�Bk�Bm�Bq�Bv�By�Bz�By�By�Bz�B�B�1B�=B�PB�\B��B��B��B��B��B��B��B��B��B�9B�LB�RB�XB�dB�dB�jB�qB�wB��B��B��BBŢBŢBĜBƨBƨBƨBƨB��B��B�B�#B�;B�BB�5B�/B�/B�TB�NB�NB�HB�NB�ZB�sB�sB�yB�yB�yB�B�B�B�B��B��B��B	B	B	%B		7B	JB	JB	VB	hB	oB	uB	{B	�B	�B	�B	$�B	+B	2-B	5?B	6FB	9XB	<jB	=qB	=qB	?}B	A�B	A�B	C�B	A�B	?}B	<jB	;dB	9XB	:^B	<jB	<jB	<jB	=qB	>wB	@�B	B�B	C�B	I�B	L�B	L�B	M�B	N�B	O�B	S�B	YB	]/B	aHB	`BB	]/B	^5B	_;B	aHB	dZB	iyB	l�B	n�B	p�B	p�B	o�B	n�B	n�B	o�B	q�B	s�B	t�B	t�B	s�B	s�B	v�B	v�B	z�B	}�B	~�B	~�B	~�B	� B	� B	� B	� B	�B	�B	�1B	�VB	�\B	�bB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�!B	�B	�B	�B	�B	�!B	�'B	�?B	�dB	�^B	�jB	��B	B	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�)B	�/B	�/B	�/B	�/B	�5B	�;B	�HB	�TB	�ZB	�ZB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
	7B
	7B
DB
DB
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
VB
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
{B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
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
.B
.B
.B
.B
/B
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
1'B
2-B
2-B
2-B
33B
33B
49B
49B
49B
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
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
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
J�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
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
T�B
T�B
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
YB
YB
YB
YB
YB
YB
YB
YB
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
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
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
bNB
bNB
cTB
cTB
cTB
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
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gm11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B8lB8lB8lB8lB8lB8�B9rB:xB;B<�B=�B@�BC�BF�BFBF?BG�BL~BSuBo�BkQBa�BG�B.IBvB�[B��B�B�TB�TB�nB�kB��B�@B�(B�rBuB\BK�B2�B(�BxB��B��B�3B��B��B�*B��B��B��B�bB��B�	B��Bx�Bd@B\�BY�BXEBTaBLBGzB=�B5%B0�B+�B(�B$�BpBB�B
	B
�]B
��B
�B
�B
ߊB
ՁB
�pB
��B
��B
��B
�B
��B
��B
�@B
�}B
�7B
�?B
{�B
y	B
tB
k�B
d�B
[qB
T�B
J�B
B�B
3�B
,�B
!�B
qB
�B
B
oB
�B	�jB	�9B	�;B	�0B	�B	�sB	ΊB	�DB	ɆB	�RB	��B	�+B	�CB	��B	��B	�B	~�B	{�B	|�B	z�B	x�B	vFB	l�B	h�B	dZB	abB	^5B	[#B	Y1B	U�B	OBB	DB	5ZB	.�B	-)B	*�B	%�B	!-B	B	�B		RB	�B	 �B��B��B��B�fB�B�B�B��B�AB�OB�B� B��B�B��B�B��B��B��BԕBуBЗB�B�B�fB��B�BB�jB�DB��B��B��B��B��B��B��B��B�B��B��B��B��B��B�eB��B��B�xB�fB�YB��B� B~BB}�B{�By�Bx�Bu�Bs�Br�Bq�BnBnBlBl"BlWBj0Bi_BhsBiyBbhB`B^�B^�B[=BX�BW�BR�BM�BL0BK)BKDBK�BK)BLBJ�BG�BE�BE�BHfBJ=BGBEBA�B;B9>B8lB7fB2�B0�B0�B0�B2�B5ZB;�B<�B;B8�B5�B1AB.B(�B&�B(�B&�B*B,�B,�B)�B(�B*KB'�B'�B+�B)�B+QB)�B(�B(�B(�B'�B(sB'mB'�B($B(�B*0B*B)�B*B+�B+�B+�B-]B/�B.cB.cB.}B.�B/�B0�B2B3�B4�B4�B6�B6B88B:�B=�B?}BAoBB�BD3BDBEBE9BFYBH�BJ�BL0BMPBNpBO�BQ�BRoBSuBS�BT�BV�BWsBW�BXB\B^�Ba�Ba�Bb�BcBd�Bi*Bk�Bk�Bk�Bj�Bk�BlBn/Br-Bv�Bz^B{dBzDBzDB{B�{B�KB��B��B��B�9B�B�:B�`B�2B�>B�sB��B�B�nB��B��B��B��B��B��B��B��B�B��B�'B�{B�YB�B��B�B�B��B��B�B�FB֡B�B�B��BޞB��B�OB�ZB��B�B�4B�nB�,B�B�B�B�B�B��B��B�B��B��B��B��B	;B	[B	tB		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	$�B	+6B	2�B	5�B	6zB	9rB	<�B	=�B	=�B	?�B	A�B	A�B	DgB	B�B	@�B	="B	;�B	9�B	:�B	<�B	<�B	<�B	=�B	>�B	@�B	B�B	DB	J	B	MB	M6B	N<B	OB	O�B	S�B	YB	]~B	bNB	`�B	]�B	^jB	_;B	aHB	d@B	i�B	l�B	n�B	qB	q'B	o�B	n�B	n�B	o�B	q�B	s�B	u%B	u%B	tB	s�B	w2B	w2B	{JB	~B	.B	HB	HB	�B	�B	�B	�4B	� B	�B	��B	��B	��B	��B	�vB	�bB	��B	�gB	�9B	��B	��B	��B	�!B	��B	��B	�B	�B	�FB	�8B	�B	�$B	�DB	�eB	�B	�KB	�B	��B	�DB	�0B	�=B	�cB	��B	��B	��B	��B	��B	�cB	�5B	�!B	�'B	��B	��B	��B	��B	��B	�uB	��B	��B	�B	�~B	οB	�jB	�B	��B	��B	��B	��B	�B	�&B	�&B	�,B	�2B	�SB	�YB	�_B	�QB	�]B	�]B	�dB	�dB	�IB	�~B	ޞB	ߤB	��B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	�B	�B	�$B	�B	�B	�B	��B	�B	�B	�6B	�<B	�(B	�.B	�HB	�.B
 4B
 iB
UB
AB
GB
GB
GB
MB
MB
gB
YB
_B
EB
_B
fB
�B
	lB
	�B
xB
^B
^B
xB
xB
�B
~B
~B
�B
�B
�B
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
B
B
 B
�B
!B
!B
!�B
!�B
!�B
!�B
#B
$B
$&B
$ZB
%`B
'RB
($B
(
B
(>B
(>B
)*B
)*B
)*B
*KB
+QB
,WB
,=B
-CB
-CB
-CB
.IB
.IB
./B
.IB
.IB
.cB
.IB
/OB
/OB
0;B
0UB
0UB
0UB
1[B
1AB
1AB
1AB
1[B
1[B
2aB
2aB
2aB
3hB
3hB
4TB
4TB
4nB
4nB
4nB
4nB
5tB
5tB
5tB
5ZB
5tB
5tB
5tB
5tB
6`B
6`B
6zB
6zB
6zB
6zB
7�B
7�B
7�B
7�B
8�B
8lB
8�B
8�B
9�B
9�B
9�B
:�B
:�B
:�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
GB
GB
G�B
G�B
G�B
H�B
H�B
H�B
IB
IB
H�B
H�B
H�B
H�B
I�B
J	B
KB
KB
J�B
J�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
MB
MB
MB
NB
NB
OB
OB
OB
O�B
PB
PB
Q B
QB
Q4B
QB
QB
Q B
QB
R B
R B
R:B
RTB
S[B
S@B
S&B
S&B
S@B
TFB
T,B
TB
TFB
T,B
UB
UB
T�B
UB
UB
UB
UB
U2B
VB
VB
VB
V9B
V9B
V9B
V9B
W?B
VSB
W?B
W?B
W?B
W$B
W?B
W?B
WYB
XEB
X_B
YB
YKB
YKB
YeB
YKB
YKB
YKB
YeB
YKB
Y1B
YKB
YKB
YKB
YeB
ZQB
ZQB
ZkB
ZQB
[WB
[=B
[WB
[�B
[qB
[=B
\xB
]IB
]dB
]IB
]dB
]dB
]dB
]dB
^jB
^jB
^OB
^OB
_VB
_VB
_pB
_VB
_VB
_VB
_VB
_VB
_�B
_pB
_�B
`vB
`\B
`vB
`vB
`vB
a�B
a�B
b�B
bhB
b�B
bhB
cnB
c�B
cnB
cnB
cnB
cnB
c�B
c�B
c�B
c�B
dtB
d�B
ezB
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<L��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.21(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201608080036032016080800360320160808003603201806221211592018062212115920180622121159201804050404252018040504042520180405040425  JA  ARFMdecpA19c                                                                20160804093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160804003525  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160804003526  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160804003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160804003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160804003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160804003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160804003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160804003528  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160804003529                      G�O�G�O�G�O�                JA  ARUP                                                                        20160804012116                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160804153852  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20160807153603  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160807153603  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190425  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031159  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101518                      G�O�G�O�G�O�                