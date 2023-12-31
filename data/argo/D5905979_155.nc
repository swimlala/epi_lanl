CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T14:00:58Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619140058  20220204114426  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�����Z1   @��}'�@5ȴ9Xb�bݑhr�!1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D� RD�` D���D��D�%�D�`RD���D�˅D��D�S�D��)D��
D�qD�NDڗ\D���D��D�VD�
D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@�G�@�G�A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C��C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?��CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY��C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg��Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt��Dy�{D��D�YHD��)D��gD�D�Y�D��D���D�D�L�D��qD��RD��D�G\Dڐ�D��D�D�O\D�RD��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA�^5A�dZA�dZA�dZA�ffA�hsA�hsA�dZA�bNA�^5A�XA�Q�A�K�A��
A�~�A���A��`A�-A��
A��7A�/A��A���A�7LA��A�p�A�7LA��A��!A�hsA�"�A��HA��A�?}A�A�r�A�;dA���A��jA��-A���A���A��\A�A�A��A���A�ƨA��7A�K�A�"�A�oA��;A��-A���A�r�A�C�A�9XA�/A�$�A��A�VA�VA�%A�%A�  A���A��mA��HA���A�A��FA��A���A���A���A���A��PA��+A��A�t�A�l�A�dZA�O�A�=qA�1'A��A�VA�  A��#A�A��FA���A�`BA���A�1'A�VA��jA�JA�-A���A��A�z�A��A��A��A��9A���A�jA�(�A��A�t�A�\)A�^5A��A�7LA�%A���A�VA�A�A�bA���A�C�A���A���A�VA��`A�~�A��^A�/A��A�1A��PA��HA�I�A���A�r�A~��A}S�A{�PAxz�AudZAr �An�/Am`BAk�AiVAg�AeXAaXA\��AX-AVv�AR�AP��AO��AM�AK33AI�AI�AG��AD�RAA�A?�;A?�A>�!A=?}A<bNA;K�A:�!A9x�A8ffA6Q�A4^5A2�+A1�A/\)A.-A-�wA,ȴA+�wA*$�A(�9A'dZA'A&��A%�;A#��A"VA!7LA   A�7A�uA�A9XA�wA+AĜA-A\)A�-A�RA�DA��A�AO�A�A��A��AdZAt�A
�A
�A	p�A	��A	/A	�A	
=A�`A�9A~�A�PA�A�-A�A`BA�uA��A33A I�A @�;d@��T@��`@�"�@��h@��w@�V@��h@��@�Z@�33@�7@���@�=q@�hs@�z�@�S�@�j@��#@�J@ߥ�@��y@�v�@ݙ�@�bN@��m@��T@�x�@���@׾w@ՙ�@ӥ�@�S�@��@�v�@�p�@���@˾w@��@��
@�ff@Ł@°!@�&�@��@���@�t�@�ȴ@�@�x�@�G�@�/@���@���@�ƨ@�+@��H@��R@�~�@�hs@���@�1@�V@��@�p�@��@���@��@�l�@��@�E�@�O�@���@���@�j@��F@���@�M�@���@�?}@���@�9X@��;@�;d@�n�@��@��T@��@�X@���@���@���@�
=@���@�ff@�5?@�@��@��@���@���@�Q�@��@���@�"�@���@�V@�=q@��@���@�p�@�7L@��@��`@��/@��/@��@��;@��@�K�@��y@���@���@��R@���@�v�@��@�@���@�p�@�X@�?}@���@�r�@�j@�1'@��m@��@�C�@�o@��\@�M�@�{@���@�p�@�X@�&�@�%@���@��D@�Z@� �@��
@��@���@���@�;d@��y@�ȴ@�~�@�=q@��^@��-@���@�?}@�Ĝ@�  @��P@�K�@�@���@�-@��@��@��j@�j@�I�@�(�@��
@�ƨ@��@�\)@�
=@��H@��@��@���@�ff@�-@���@�@��@�7L@�&�@���@��u@�1'@�1@��@��F@�\)@�33@�ȴ@��@��-@��-@�X@�?}@�&�@��@�V@�V@�%@��/@��j@��j@��9@��@�j@�I�@�9X@�(�@� �@��@�b@���@��w@��w@��w@��F@�|�@�o@���@���@���@���@���@���@��\@�v�@�{@���@��-@��7@�hs@�G�@�V@���@�r�@�(�@� �@��@��@���@�C�@�33@��@��+@�z@w�6@rff@j��@d_@\b@T��@L��@D�|@>i�@7;d@0�9@+�a@&�@"��@�#@h
@�@8@_@�~111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�VA�^5A�dZA�dZA�dZA�ffA�hsA�hsA�dZA�bNA�^5A�XA�Q�A�K�A��
A�~�A���A��`A�-A��
A��7A�/A��A���A�7LA��A�p�A�7LA��A��!A�hsA�"�A��HA��A�?}A�A�r�A�;dA���A��jA��-A���A���A��\A�A�A��A���A�ƨA��7A�K�A�"�A�oA��;A��-A���A�r�A�C�A�9XA�/A�$�A��A�VA�VA�%A�%A�  A���A��mA��HA���A�A��FA��A���A���A���A���A��PA��+A��A�t�A�l�A�dZA�O�A�=qA�1'A��A�VA�  A��#A�A��FA���A�`BA���A�1'A�VA��jA�JA�-A���A��A�z�A��A��A��A��9A���A�jA�(�A��A�t�A�\)A�^5A��A�7LA�%A���A�VA�A�A�bA���A�C�A���A���A�VA��`A�~�A��^A�/A��A�1A��PA��HA�I�A���A�r�A~��A}S�A{�PAxz�AudZAr �An�/Am`BAk�AiVAg�AeXAaXA\��AX-AVv�AR�AP��AO��AM�AK33AI�AI�AG��AD�RAA�A?�;A?�A>�!A=?}A<bNA;K�A:�!A9x�A8ffA6Q�A4^5A2�+A1�A/\)A.-A-�wA,ȴA+�wA*$�A(�9A'dZA'A&��A%�;A#��A"VA!7LA   A�7A�uA�A9XA�wA+AĜA-A\)A�-A�RA�DA��A�AO�A�A��A��AdZAt�A
�A
�A	p�A	��A	/A	�A	
=A�`A�9A~�A�PA�A�-A�A`BA�uA��A33A I�A @�;d@��T@��`@�"�@��h@��w@�V@��h@��@�Z@�33@�7@���@�=q@�hs@�z�@�S�@�j@��#@�J@ߥ�@��y@�v�@ݙ�@�bN@��m@��T@�x�@���@׾w@ՙ�@ӥ�@�S�@��@�v�@�p�@���@˾w@��@��
@�ff@Ł@°!@�&�@��@���@�t�@�ȴ@�@�x�@�G�@�/@���@���@�ƨ@�+@��H@��R@�~�@�hs@���@�1@�V@��@�p�@��@���@��@�l�@��@�E�@�O�@���@���@�j@��F@���@�M�@���@�?}@���@�9X@��;@�;d@�n�@��@��T@��@�X@���@���@���@�
=@���@�ff@�5?@�@��@��@���@���@�Q�@��@���@�"�@���@�V@�=q@��@���@�p�@�7L@��@��`@��/@��/@��@��;@��@�K�@��y@���@���@��R@���@�v�@��@�@���@�p�@�X@�?}@���@�r�@�j@�1'@��m@��@�C�@�o@��\@�M�@�{@���@�p�@�X@�&�@�%@���@��D@�Z@� �@��
@��@���@���@�;d@��y@�ȴ@�~�@�=q@��^@��-@���@�?}@�Ĝ@�  @��P@�K�@�@���@�-@��@��@��j@�j@�I�@�(�@��
@�ƨ@��@�\)@�
=@��H@��@��@���@�ff@�-@���@�@��@�7L@�&�@���@��u@�1'@�1@��@��F@�\)@�33@�ȴ@��@��-@��-@�X@�?}@�&�@��@�V@�V@�%@��/@��j@��j@��9@��@�j@�I�@�9X@�(�@� �@��@�b@���@��w@��w@��w@��F@�|�@�o@���@���@���@���@���@���@��\@�v�@�{@���@��-@��7@�hs@�G�@�V@���@�r�@�(�@� �@��@��@���@�C�@�33@��G�O�@�z@w�6@rff@j��@d_@\b@T��@L��@D�|@>i�@7;d@0�9@+�a@&�@"��@�#@h
@�@8@_@�~111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
E�B
E�B
E�B
D�B
K�B
W
B
D�B
9XB
<jB
;dB
:^B
7LB
8RB
9XB
9XB
8RB
9XB
=qB
F�B
S�B
_;B
gmB
v�B
�7B
��B
��B
��B
��B
�9B
�}B
ŢB
��B
��B
��B
�)B
�TB
�sB
�yB
�B
�B
��B
��B  B%B
=BVBoB�B�B�B�B�B�B�B�B�B �B"�B#�B#�B#�B#�B#�B#�B#�B$�B$�B%�B%�B%�B'�B(�B(�B/B8RB>wBH�BO�BXBk�Bu�B{�B� B��BǮB�B�B��B��BB	7BVB�B�B!�B!�B�B!�B�B�BhBPBB��B�B�BB�B��B�dB�B��Bo�BP�BE�B6FB1'B+B!�BuB
�B
ÖB
��B
��B
�JB
~�B
s�B
P�B
;dB
/B
!�B
1B	�B	��B	�qB	�B	��B	�PB	|�B	n�B	S�B	6FB	�B	JB��B�B�sB�fB�B��BǮB�}B�9B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�JB�1B�B�B~�B~�B|�Bz�Bw�Bw�Bw�Bt�Br�Bo�Bk�B`BB\)B_;B_;B]/BXBW
BW
BffBhsBdZBm�Bs�Bo�Bt�Bv�B{�B{�Bu�Bl�BhsBjBjBr�Br�Br�Bs�Bs�Bt�Bu�Bw�Bv�Bt�Br�Bp�Bo�Bu�Bm�Bn�Bn�Bo�Bn�Bn�Bm�Bm�Bn�Bn�Bn�Bm�Bn�Bm�Bm�Bl�Bl�Bk�BjBiyBiyBgmBffBffBdZBdZBffBm�Bo�Bs�Bt�Bu�Bu�Bw�Bv�Bv�Bv�Bw�By�Bz�Bu�Bq�Bl�BhsBgmBhsBjBm�Bm�Bl�Bn�Br�Bs�Bu�Bv�Bw�By�B|�B~�B�B�B�B�+B�B�B�7B�=B�DB�JB�PB�\B�oB�{B��B��B��B��B��B��B�B�B�'B�3B�LB�XB�^B�wB��BBÖBĜBŢBǮBȴB��B��B��B��B��B�
B�/B�HB�fB�yB�B�B�B��B��B��B��B��B	B	B	B	B	1B		7B	
=B	VB	uB	{B	�B	�B	�B	�B	 �B	#�B	$�B	(�B	)�B	,B	/B	/B	0!B	49B	9XB	:^B	>wB	A�B	D�B	E�B	H�B	M�B	O�B	Q�B	VB	YB	[#B	]/B	_;B	aHB	dZB	ffB	hsB	k�B	l�B	m�B	m�B	r�B	t�B	v�B	y�B	{�B	� B	� B	� B	�B	�B	�B	�%B	�7B	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�?B	�FB	�LB	�XB	�dB	�jB	�jB	�wB	�}B	��B	��B	��B	��B	��B	B	B	B	ÖB	ŢB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�BB	�NB	�TB	�ZB	�ZB	�fB	�sB	�yB	�yB	�B	�B	�+B
B
kB
�B
%�B
/ B
9	B
C�B
J�B
QhB
W�B
\xB
`'B
dtB
g�B
m]B
p;B
uB
w�B
z^B
|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
5�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
5�B
5�B
5�B
5�B
4�B
<B
GRB
4�B
)�B
,�B
+�B
*�B
'�B
(�B
)�B
)�B
(�B
)�B
-�B
6�B
DCB
O�B
W�B
gB
y|B
��B
�B
�B
�>B
�zB
��B
��B
��B
�B
�/B
�eB
ӏB
خB
ٴB
��B
��B
�B
�B
�8B
�]B
�tB
��B�B�B�B�B	�B�B�B�B�B�B�BBBBBBBBBBBBBB$B*B*BNB(�B.�B8�B@BH>B[�Be�BlBp*B��B��B�0BܩB��B�
B�:B�RB�qB�B	�B�B�B�B�B�B	�B�B�oB�9B�	B��B�hB�>B��B��B�0B��B_�BA B5�B&�B!fBBBB�B
��B
��B
�B
��B
|�B
oSB
dB
ADB
+�B
B
1B	��B	�B	�oB	��B	�~B	�0B	}�B	mkB	_B	D{B	&�B	B��B�rB�$B�B��BɭB�eB�GB�B��B��B�:B�(B�(B�"B�"B�MB�MB�lB�fB�lB�NB�CB�%B|�Bx�Bt�Bq�Bo�Bo�Bm�Bk�BhyBhyBhyBegBc[B`JB\2BP�BL�BO�BO�BM�BH�BG�BG�BWBY"BU
B^@BdeB`MBekBgxBl�Bl�BfsB]<BY%B[1B[1BcaBcaBcaBdgBdgBemBftBh�BgzBenBcbBaVB`QBfuB^EB_LB_LB`RB_LB_LB^FB^FB_MB_MB_MB^FB_MB^FB^GB]AB]AB\;B[6BZ0BZ0BX%BWBWBUBUBWB^IB`VBdnBetBf{Bf{Bh�Bg�Bg�Bg�Bh�Bj�Bk�Bf|BbdB]FBY/BX)BY/B[;B^MB^MB]GB_TBclBdrBf~Bg�Bh�Bj�Bm�Bo�Bq�Bq�Bs�Bw�Bu�Bu�By�Bz�B{�B}B~
B�B�(B�4B�LB�jB�vB�}B��B��B��B��B��B��B�B�B�B�,B�>B�DB�KB�QB�WB�bB�hB��B��B B BīBǽB��B��B�B�*B�6B�TB�`B�lB�B�B�B�B�B��B��B��B��B��B��B�B	"B	(B	9B	
FB	RB	^B	pB	�B	�B	�B	�B	�B	�B	�B	 �B	$�B	*B	+B	/B	21B	5DB	6IB	9[B	>zB	@�B	B�B	F�B	I�B	K�B	M�B	O�B	Q�B	T�B	W
B	YB	\(B	].B	^4B	^4B	cSB	e^B	gkB	j}B	l�B	p�B	p�B	p�B	s�B	u�B	u�B	v�B	y�B	~�B	�B	�B	� B	�&B	�?B	�\B	�cB	�iB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�$B	�$B	�*B	�*B	�*B	�1B	�=B	�IB	�IB	�NB	�gB	�yB	�yB	�~B	B	B	B	B	B	B	B	B	B	B	B	B	ËB	ËB	ËB	ËB	đB	ŗB	ǣB	ȩB	ȩB	ȩB	ȩB	ɰB	ɰB	ɰB	ɰB	ʶB	˼B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�G�O�B	��B	��B

�B
7B
[B
�B
)�B
4�B
;B
A�B
HkB
MB
P�B
UB
XB
]�B
`�B
e�B
h\B
j�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.21 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.015(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144262022020411442620220204114426  AO  ARCAADJP                                                                    20200619140058    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619140058  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619140058  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114426  IP                  G�O�G�O�G�O�                