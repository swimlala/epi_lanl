CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:22Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170922  20220204114423  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               zA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���<��1   @����}7@6���$��b����o1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    zA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�)D�*�D�[�D���D�� D��D�\�D��D��D�)D�P D��qD��D�qD�W�Dڪ�D�ָD� D�N�D��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@�G�@�G�A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B�ǮBϔ{BӔ{Bה{B۔{Bߔ{B�ǮB�{B�{B�{B�{B��{B��{B��{C�=C�=C��C�=C	�=C��C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��RD r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D l)D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7x�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�)D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHx�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�DtҏDy��D�#�D�T�D��>D��HD��\D�VD��gD���D�qD�IHD���D���D��D�P�Dڤ)D�� D�	HD�H D�)D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�M�A�O�A�Q�A�O�A�Q�A�Q�A�S�A�XA�\)A�^5A�`BA�`BA�bNA�dZA�hsA�p�A�r�A�t�A�x�A�z�A�z�A�|�A�~�A�~�AˁAˁA˃A˅Aˇ+Aˇ+Aˇ+Aˇ+Aˇ+Aˇ+Aˇ+Aˇ+Aˉ7Aˇ+Aˇ+Aˇ+Aˇ+Aˇ+Aˇ+A˅A˃A˃A˃A˃A�~�A�v�A�l�A�dZA�;dA�7LA�A��A�O�A�?}A��HA��A��\A�33A��/A�VA�G�A�E�A��9A�1A���A��A��A���A���A��PA�=qA�bA��A��A�{A��A��DA�bA�^5A���A�r�A���A�
=A�G�A���A�"�A�O�A�/A��#A��-A�&�A��A�`BA�t�A��uA��jA��A�hsA�JA�33A�A�1A��!A���A�^5A��A���A���A��A�A�A���A�1A�S�A�^5A��A~�9A{�Az$�AwƨArQ�AqS�Ap��Akt�Ag&�Ad�DAb�yAa�A_oA\9XAZ��AW�AU�AR�yAP�AN��AM�AL��AK?}AJ�AH��AG7LAE�7AD��AC��AC��AB�AB1A@��A?��A?t�A>�A=/A<�A:��A9�A8�!A7��A6ȴA6$�A5;dA3`BA1��A1S�A0E�A/
=A-`BA,�jA+|�A*ZA)�TA)�hA(I�A'�A%��A#��A#
=A"~�A!��A =qA�mA�AE�A+A��Ax�A�+A�;AXAXAQ�AI�A+A\)A-A�-Ar�A��A9XA-A�7AJAl�AZAjAoAr�A��AXAA+AE�AC�A
ZA�A$�AG�AoA�A ��@�V@��j@�V@��@���@���@���@�D@��@�-@�A�@�@��@��@�R@�@�A�@�t�@�K�@�;d@�33@߾w@��@�v�@�~�@�j@ۅ@��@ڟ�@�@�1'@�dZ@֗�@���@�`B@��@��@�r�@� �@�9X@��@�
=@ёh@�  @͉7@ʟ�@�"�@�|�@˥�@ʰ!@�$�@�O�@�/@��@ř�@�r�@�@�Z@�  @��
@���@��@��@�-@��@��j@�=q@���@�  @�|�@��\@���@�1'@�t�@���@���@��@�bN@��w@�ff@��@��@�  @���@�~�@��!@��@�|�@��
@���@��#@�z�@�S�@��R@��\@�5?@���@�%@�V@�G�@�@�@�V@���@�@���@�@�`B@���@���@�V@��@�l�@�;d@�@��F@��u@��9@�Q�@�+@�K�@�;d@�ȴ@�^5@���@�V@��9@���@��w@��@�b@��;@���@�;d@�o@���@�{@�@�?}@�V@��/@�9X@�ƨ@��y@��!@��!@��\@�$�@��@��#@��-@��h@�G�@���@��/@��`@��`@��j@�I�@���@��@�+@�o@���@�^5@�M�@�{@��T@��-@�V@�bN@���@���@���@���@���@�ƨ@�|�@��@�{@�G�@��D@��D@�I�@��@���@�t�@�E�@��T@�@��@�V@��@�G�@�p�@��/@�G�@��@���@�t�@��m@�;d@��@���@�E�@�@��7@�p�@�hs@�G�@��@�Ĝ@��u@�  @��w@���@��P@�K�@��@���@��R@���@���@��@���@�$�@�{@�J@�J@�@���@���@��T@��-@���@�p�@�V@�I�@�  @� �@�1'@�(�@��@�1@���@�+@�v�@�=q@��@��@��#@���@�hs@�/@���@��@�Z@�I�@��F@�dZ@�S�@�33@��@��@���@�v�@��@x$@o�@g�@a�@[@O@S�
@J�s@DQ�@?=@9}�@3\)@,�.@(M@#�@��@��@��@�1@�@
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�M�A�O�A�Q�A�O�A�Q�A�Q�A�S�A�XA�\)A�^5A�`BA�`BA�bNA�dZA�hsA�p�A�r�A�t�A�x�A�z�A�z�A�|�A�~�A�~�AˁAˁA˃A˅Aˇ+Aˇ+Aˇ+Aˇ+Aˇ+Aˇ+Aˇ+Aˇ+Aˉ7Aˇ+Aˇ+Aˇ+Aˇ+Aˇ+Aˇ+A˅A˃A˃A˃A˃A�~�A�v�A�l�A�dZA�;dA�7LA�A��A�O�A�?}A��HA��A��\A�33A��/A�VA�G�A�E�A��9A�1A���A��A��A���A���A��PA�=qA�bA��A��A�{A��A��DA�bA�^5A���A�r�A���A�
=A�G�A���A�"�A�O�A�/A��#A��-A�&�A��A�`BA�t�A��uA��jA��A�hsA�JA�33A�A�1A��!A���A�^5A��A���A���A��A�A�A���A�1A�S�A�^5A��A~�9A{�Az$�AwƨArQ�AqS�Ap��Akt�Ag&�Ad�DAb�yAa�A_oA\9XAZ��AW�AU�AR�yAP�AN��AM�AL��AK?}AJ�AH��AG7LAE�7AD��AC��AC��AB�AB1A@��A?��A?t�A>�A=/A<�A:��A9�A8�!A7��A6ȴA6$�A5;dA3`BA1��A1S�A0E�A/
=A-`BA,�jA+|�A*ZA)�TA)�hA(I�A'�A%��A#��A#
=A"~�A!��A =qA�mA�AE�A+A��Ax�A�+A�;AXAXAQ�AI�A+A\)A-A�-Ar�A��A9XA-A�7AJAl�AZAjAoAr�A��AXAA+AE�AC�A
ZA�A$�AG�AoA�A ��@�V@��j@�V@��@���@���@���@�D@��@�-@�A�@�@��@��@�R@�@�A�@�t�@�K�@�;d@�33@߾w@��@�v�@�~�@�j@ۅ@��@ڟ�@�@�1'@�dZ@֗�@���@�`B@��@��@�r�@� �@�9X@��@�
=@ёh@�  @͉7@ʟ�@�"�@�|�@˥�@ʰ!@�$�@�O�@�/@��@ř�@�r�@�@�Z@�  @��
@���@��@��@�-@��@��j@�=q@���@�  @�|�@��\@���@�1'@�t�@���@���@��@�bN@��w@�ff@��@��@�  @���@�~�@��!@��@�|�@��
@���@��#@�z�@�S�@��R@��\@�5?@���@�%@�V@�G�@�@�@�V@���@�@���@�@�`B@���@���@�V@��@�l�@�;d@�@��F@��u@��9@�Q�@�+@�K�@�;d@�ȴ@�^5@���@�V@��9@���@��w@��@�b@��;@���@�;d@�o@���@�{@�@�?}@�V@��/@�9X@�ƨ@��y@��!@��!@��\@�$�@��@��#@��-@��h@�G�@���@��/@��`@��`@��j@�I�@���@��@�+@�o@���@�^5@�M�@�{@��T@��-@�V@�bN@���@���@���@���@���@�ƨ@�|�@��@�{@�G�@��D@��D@�I�@��@���@�t�@�E�@��T@�@��@�V@��@�G�@�p�@��/@�G�@��@���@�t�@��m@�;d@��@���@�E�@�@��7@�p�@�hs@�G�@��@�Ĝ@��u@�  @��w@���@��P@�K�@��@���@��R@���@���@��@���@�$�@�{@�J@�J@�@���@���@��T@��-@���@�p�@�V@�I�@�  @� �@�1'@�(�@��@�1@���@�+@�v�@�=q@��@��@��#@���@�hs@�/@���@��@�Z@�I�@��F@�dZ@�S�@�33@��@��@���G�O�@��@x$@o�@g�@a�@[@O@S�
@J�s@DQ�@?=@9}�@3\)@,�.@(M@#�@��@��@��@�1@�@
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
�B
�B
�B
�B
�B
�B
�!B
�'B
�-B
�-B
�3B
�9B
�?B
�9B
�9B
�?B
�?B
�?B
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�LB
�LB
�LB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�XB
�XB
�XB
�^B
�^B
�dB
ĜB
��By�B�7B�bB��B��B�BƨB�/B+B\B�B33BB�B>wB6FB"�B#�B$�B$�B2-B33B%�B�B�B!�B,B;dBQ�BW
B\)B^5BH�B2-B!�BVB��B�`B��B�!B�!B��B��B�BhsBC�B(�B�B%B
��B
�ZB
��B
�3B
��B
��B
��B
�JB
�B
o�B
VB
H�B
9XB
&�B
DB	�B	�#B	��B	�B	��B	��B	~�B	^5B	F�B	=qB	1'B	'�B	 �B	�B	DB��B�sB�BB�;B�NB�;B�B�B��B��B�B�B�B�B�B�B�
B��B��B��B��BĜB��B�jB�LB�3B�B�B��B��B��B��B��B�hB�PB�1B�+B�B�B�B~�Bz�Bw�Bp�Bl�BiyBgmBgmB`BBaHBjB�JB��B�B��B��B��B��B�qB��B�B��B��BɺB�B�B�B�B�B�yB�ZB�5B��B��BɺBB��B��B�dB�LB�!B��B{�BiyBaHB_;BYBM�BG�B=qB7LB49B49B1'B/B/B.B/B49B49B33B33B5?B6FB8RB=qB>wB>wB?}BC�BE�BF�BL�BO�BL�BK�BK�BJ�BJ�BJ�BP�BW
BW
BXB\)BaHBcTBgmBhsBm�Bn�Bm�BhsBffBk�Bp�By�B}�B�B�B�B~�B{�By�By�Bq�Bq�Bv�B|�B}�B}�B}�By�B�B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�-B�B�B�B�B�9B�?B�FB�jBÖBȴB�B��B��B��B�HB�ZB�ZB�mB�B�B�B�B�B�B��B��B	%B	+B	+B	B	DB	{B	{B	�B	�B	�B	�B	�B	�B	!�B	#�B	%�B	'�B	)�B	.B	/B	0!B	2-B	2-B	6FB	8RB	9XB	=qB	=qB	>wB	A�B	B�B	C�B	E�B	G�B	I�B	J�B	M�B	O�B	P�B	P�B	R�B	W
B	[#B	[#B	bNB	e`B	iyB	jB	l�B	n�B	r�B	s�B	u�B	u�B	w�B	y�B	{�B	z�B	{�B	�B	�B	�B	�B	�B	�B	�%B	�=B	�PB	�PB	�PB	�PB	�PB	�JB	�JB	�JB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�?B	�FB	�LB	�XB	�dB	�}B	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�;B	�;B	�BB	�BB	�NB	�TB	�TB	�B	�$B
_B
�B
=B
+QB
2�B
8�B
?B
A B
G�B
M�B
S[B
V9B
[	B
_VB
dB
g�B
l�B
raB
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
� B
� B
� B
�%B
�2B
�8B
�>B
�DB
�DB
�JB
�PB
�VB
�PB
�PB
�VB
�VB
�VB
�]B
�]B
�]B
�]B
�]B
�]B
�]B
�]B
�]B
�]B
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�nB
�nB
�nB
�nB
�tB
�tB
�zB
��B
�Bm�B}>B�hB��B��B�B��B�/B�(BXB�B'-B6�B2qB*AB�B�B�B�B&)B'/B�B�B�B�B B/`BE�BKBP#BR/B<�B&,B�BXB��B�gB��B�-B�-B�B��Bw#B\�B7�BB
�B
�CB
��B
�|B
�"B
�ZB
�B
��B
��B
�vB
xFB
c�B
J6B
<�B
-�B
 B	�~B	޼B	�bB	�B	�GB	��B	��B	sDB	R�B	:�B	1�B	%yB	CB	B	�B��B�"B��BԞBӗB֪BӘB�zB�bB�PB�,B�oB�|B�|B�vB�vB�|B�jB�XB�LB�:B�"B��B��B��B��B��B��B�mB�\B�7B�B�B��B��B��B|�B{�Bw}Bw}BuqBsfBoMBl<BeB`�B]�B[�B[�BT�BU�B^�B��B�YB�wB�AB�AB�MB�`B��B�XB�vB�FB�.B�"B΃B��B��B�B�B��B��BқB�ZB�*B�#B��B��B��B��B��B��B�BpYB]�BU�BS�BM�BBJB<&B1�B+�B(�B(�B%�B#�B#�B"�B#�B(�B(�B'�B'�B)�B*�B,�B1�B2�B2�B3�B8B:B;#BAHBDZBAHB@BB@BB?<B?=B?=BE`BK�BK�BL�BP�BU�BW�B[�B\�Bb
BcBb
B\�BZ�B_�BeBnTBrlBv�Bu~Bu~BssBp`BnTBnUBf%Bf%BkCBqhBrnBrnBrnBnUBx�B��B��B��B��B�/B�B�B�B�5B�<B�BB�ZB�sB�`B�=B�0B�$B�B�B�0B�IB�gB��B��B��B��B��B��B��B��B��B��B�B�)B�xB�fB�`B�NBջB��B��B��B��B�B�B�B�B�"B�.B�_B��B��B��B��B��B	�B	�B		�B		�B	�B	B	&B	,B	9B	EB	PB	]B	iB	"�B	#�B	$�B	&�B	&�B	*�B	,�B	-�B	1�B	1�B	2�B	5�B	6�B	8B	:B	<B	>%B	?,B	B>B	DIB	EOB	EOB	G\B	KtB	O�B	O�B	V�B	Y�B	]�B	^�B	`�B	c B	gB	hB	j*B	j+B	l6B	nBB	pNB	oHB	pNB	vsB	xB	y�B	y�B	x�B	x�B	z�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�B	�B	�B	�5B	�;B	�;B	�AB	�AB	�GB	�MB	�TB	�ZB	�fB	�fB	�fB	�lB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�.B	�4B	�4B	�4B	�/B	�@B	�FB	�SB	�YB	�YB	�YB	�_B	�YB	�YB	�_B	�eB	�kB	�xB	�xB	�xB	τB	ЉB	ЉB	яB	ҕB	ӛB	ӛB	ԢB	ԢB	֮B	״G�O�B	��B	�B	��B
#B
�B
�B
'@B
-0B
3oB
5{B
;�B
A�B
G�B
J�B
OcB
S�B
XdB
[�B
aMB
f�B
i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.21 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.011(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144232022020411442320220204114423  AO  ARCAADJP                                                                    20200619170922    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170922  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170922  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114423  IP                  G�O�G�O�G�O�                