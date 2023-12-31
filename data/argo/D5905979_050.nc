CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:05Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170905  20220204114415  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               2A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؜��R[g1   @؜�F)��@5���+�c��1'1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    2A   B   B   @���@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�33A�33B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN�fDOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DVy�DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D��D�J�D���D���D�2=D�_�D���D���D��D�K3D���D��\D��D�T�Dڔ)D���D��D�T{D�)D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@�G�@�G�A��A<��A\��A|��A�Q�A��A�Q�A�Q�A�Q�A݅A�A�Q�B(�B(�B�\B(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B�aHB��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs��Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dl)D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9x�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM��DNx�DN��DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVl)DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da��Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�\Dy�RD��D�C�D��D��4D�+�D�X�D���D��D�4D�D{D�� D�ФD��D�NDڍqD��D��D�M�D�qD��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Að!Aô9AøRAüjAþwAüjAú^A���A���A���AÓuA�^5A�A�hsA�5?A��A��A�VA�A�{A�A��A��yA��;A��A��^A���A��uA��7A��+A��A�l�A�dZA�M�A�33A��A�
=A�A�  A��A��A��TA��+A��DA���A�p�A�ffA��/A�VA�v�A��A���A���A��A��A���A�1A���A���A�jA�l�A�/A���A�?}A�jA��\A��A��jA�Q�A��#A���A���A�A�A�JA��/A��!A�~�A��HA���A�/A�Q�A�=qA�bA��#A�$�A�VA���A�ĜA�
=A���A�;dA��A�hsA�(�A�ffA�JA�$�A�dZA��uA�bNA�A�dZA��PA�S�A�A�A��+A�|�A���A�v�A��A��TA�VA~�!A|�RA{��Az�RAy�
Aw�TAvM�As�mAqK�AnVAm/AlI�Ak&�Ai��Ae��AdQ�Ac�AaG�A_��A_/A^ĜA^ĜA^��A^��A^~�A^bNA^�A]K�A\r�AZ��AV�HAS��AR��ARr�AQ\)AOANA�AJ�HAH  AE�PAD  AC�AA�FA@M�A>~�A<bNA;7LA:��A:5?A9XA8jA7�FA7C�A6E�A4ȴA3�^A3oA0^5A.��A.1A-��A-K�A,^5A+��A+��A*^5A*bA)|�A(�A(ZA'�A&A�A%�FA%C�A$�jA$1'A#�;A#33A"1A!XA n�A��A�+At�Az�A�wAG�A�jA^5A�mA
=A�A1A�uA1'A1'A �A��A�#A~�At�A�/A�A"�A��A~�AQ�Ap�A^5A ��A J@��@���@�%@���@��@�(�@�K�@��\@���@���@�|�@�$�@���@�@�v�@�j@�-@�hs@�@�ƨ@⟾@�7L@�1'@߶F@���@�p�@��;@�X@�ƨ@��@�v�@�V@�{@�%@ӶF@��`@�"�@��@�z�@�Q�@��@�ƨ@��@�7L@�dZ@Ƨ�@�@�z�@Å@���@�%@��/@���@��@�ƨ@���@�p�@�1@�{@�b@�l�@��!@�X@��@�bN@� �@��P@��\@���@�p�@�Z@��@�b@�1@��@��w@��F@��@��@�o@��R@�$�@���@���@���@�O�@��u@��;@��@��@�\)@��@�~�@�$�@��@���@��7@�p�@��@�Z@� �@� �@� �@��@���@��
@��F@���@�C�@�ȴ@��\@�$�@��#@�7L@��`@��@�Z@�9X@� �@��;@��@�C�@�33@�33@�;d@��@���@�ȴ@��!@�=q@�{@�@�hs@��@��D@�A�@�(�@�ƨ@���@�ff@�=q@���@��#@�@�@�@��-@�?}@�9X@���@���@���@���@�9X@�+@�p�@��@��/@�Z@�1'@� �@�b@�  @��w@�S�@�;d@�"�@��@��\@�=q@��@�{@���@���@���@�@�@��@���@��^@��-@���@���@��@�`B@��@�7L@�r�@�1'@���@��
@��P@�S�@�33@���@�-@���@��@��T@���@�`B@�/@��@�%@�Ĝ@�z�@�r�@�j@�9X@��;@���@���@�|�@�\)@��@�n�@��@��^@��-@�G�@���@��@��@��`@��`@��@��`@���@���@��@��u@��9@���@��D@�z�@�Q�@��m@��@�t�@�
=@���@�-@�J@�J@��T@��^@��7@�p�@���@�z�@�1'@�@~�y@~v�@~5?@}��@}`B@|�@|(�@{ƨ@{t�@{S�@{C�@z�@z��@z�\@x�@u0�@m�@e�@_�@U	l@NL0@Hg8@C)_@<��@8�@2i�@+��@'�$@"�@��@&�@��@
v�@o@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Að!Aô9AøRAüjAþwAüjAú^A���A���A���AÓuA�^5A�A�hsA�5?A��A��A�VA�A�{A�A��A��yA��;A��A��^A���A��uA��7A��+A��A�l�A�dZA�M�A�33A��A�
=A�A�  A��A��A��TA��+A��DA���A�p�A�ffA��/A�VA�v�A��A���A���A��A��A���A�1A���A���A�jA�l�A�/A���A�?}A�jA��\A��A��jA�Q�A��#A���A���A�A�A�JA��/A��!A�~�A��HA���A�/A�Q�A�=qA�bA��#A�$�A�VA���A�ĜA�
=A���A�;dA��A�hsA�(�A�ffA�JA�$�A�dZA��uA�bNA�A�dZA��PA�S�A�A�A��+A�|�A���A�v�A��A��TA�VA~�!A|�RA{��Az�RAy�
Aw�TAvM�As�mAqK�AnVAm/AlI�Ak&�Ai��Ae��AdQ�Ac�AaG�A_��A_/A^ĜA^ĜA^��A^��A^~�A^bNA^�A]K�A\r�AZ��AV�HAS��AR��ARr�AQ\)AOANA�AJ�HAH  AE�PAD  AC�AA�FA@M�A>~�A<bNA;7LA:��A:5?A9XA8jA7�FA7C�A6E�A4ȴA3�^A3oA0^5A.��A.1A-��A-K�A,^5A+��A+��A*^5A*bA)|�A(�A(ZA'�A&A�A%�FA%C�A$�jA$1'A#�;A#33A"1A!XA n�A��A�+At�Az�A�wAG�A�jA^5A�mA
=A�A1A�uA1'A1'A �A��A�#A~�At�A�/A�A"�A��A~�AQ�Ap�A^5A ��A J@��@���@�%@���@��@�(�@�K�@��\@���@���@�|�@�$�@���@�@�v�@�j@�-@�hs@�@�ƨ@⟾@�7L@�1'@߶F@���@�p�@��;@�X@�ƨ@��@�v�@�V@�{@�%@ӶF@��`@�"�@��@�z�@�Q�@��@�ƨ@��@�7L@�dZ@Ƨ�@�@�z�@Å@���@�%@��/@���@��@�ƨ@���@�p�@�1@�{@�b@�l�@��!@�X@��@�bN@� �@��P@��\@���@�p�@�Z@��@�b@�1@��@��w@��F@��@��@�o@��R@�$�@���@���@���@�O�@��u@��;@��@��@�\)@��@�~�@�$�@��@���@��7@�p�@��@�Z@� �@� �@� �@��@���@��
@��F@���@�C�@�ȴ@��\@�$�@��#@�7L@��`@��@�Z@�9X@� �@��;@��@�C�@�33@�33@�;d@��@���@�ȴ@��!@�=q@�{@�@�hs@��@��D@�A�@�(�@�ƨ@���@�ff@�=q@���@��#@�@�@�@��-@�?}@�9X@���@���@���@���@�9X@�+@�p�@��@��/@�Z@�1'@� �@�b@�  @��w@�S�@�;d@�"�@��@��\@�=q@��@�{@���@���@���@�@�@��@���@��^@��-@���@���@��@�`B@��@�7L@�r�@�1'@���@��
@��P@�S�@�33@���@�-@���@��@��T@���@�`B@�/@��@�%@�Ĝ@�z�@�r�@�j@�9X@��;@���@���@�|�@�\)@��@�n�@��@��^@��-@�G�@���@��@��@��`@��`@��@��`@���@���@��@��u@��9@���@��D@�z�@�Q�@��m@��@�t�@�
=@���@�-@�J@�J@��T@��^@��7@�p�@���@�z�@�1'@�@~�y@~v�@~5?@}��@}`B@|�@|(�@{ƨ@{t�@{S�@{C�@z�@z��G�O�@x�@u0�@m�@e�@_�@U	l@NL0@Hg8@C)_@<��@8�@2i�@+��@'�$@"�@��@&�@��@
v�@o@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�dB�dB�^B�^B�^B�dB�jB�dB�dB�dBƨB��B�ZB%BuB�B�B�B �B5?BT�B_;BdZBm�Bu�B�B�VB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�=B�1B�B�B�B}�B|�B}�B{�B}�B|�B|�B� B~�B�B}�B{�B{�Bz�B{�By�Bv�Bs�Bs�Bo�Bn�Bn�BjBhsBYBW
BS�BQ�BE�B6FB#�B!�B�B�BoB��B��B�XB��B��B��B�hB�Bm�B_;BYBP�B5?B$�B �B�B\B
��B
��B
�}B
��B
��B
�oB
�JB
�B
z�B
r�B
hsB
_;B
W
B
R�B
M�B
D�B
6FB
0!B
�B
VB
B	��B	�B	�B	��B	ɺB	��B	�RB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�7B	p�B	\)B	P�B	K�B	B�B	8RB	,B	�B	1B�B�yB�NB�#B��BƨB�jB�-B�!B�B��B��B��B��B��B�bB�%B�B}�Bs�Bp�Bn�Bl�Bl�BgmBgmBaHB^5B^5B]/B_;B_;B_;B_;B_;B_;B^5B]/B]/B^5B\)B[#B[#BT�BR�BQ�BN�BM�BL�BK�BJ�BI�BG�BG�BD�B@�B@�B9XB7LB6FB5?B49B49B5?B33B33B2-B1'B2-B0!B1'B0!B0!B/B.B,B,B,B/B/B2-B1'B0!B1'B5?B33B5?B7LB8RB8RB9XB9XB:^B;dB;dB;dB<jB=qB>wBB�BB�BC�BC�BB�BB�BC�BE�BH�BM�BS�BT�BT�BT�BT�BVB[#B^5B_;BaHBiyBm�Bs�Bs�Bs�Bt�Bt�Bv�Bw�Bw�Bt�Bs�Bv�Bw�By�B�B�B�7B�hB��B��B��B��B�B�B�B�B�!B�'B�-B�-B�3B�FB�RB�^B�}BŢBǮB��B��B�B�B�B�B�;B�fB�B�B�B�B�B��B��B��B	B	B	B	B	%B	+B		7B	JB	\B	\B	oB	uB	�B	�B	�B	�B	�B	 �B	%�B	+B	2-B	49B	6FB	9XB	;dB	@�B	B�B	D�B	K�B	P�B	VB	XB	ZB	[#B	\)B	\)B	^5B	^5B	_;B	bNB	e`B	ffB	hsB	iyB	jB	jB	k�B	m�B	s�B	t�B	x�B	y�B	z�B	{�B	~�B	�B	�%B	�VB	�\B	�\B	�VB	�VB	�VB	�\B	�\B	�bB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�!B	�'B	�-B	�3B	�?B	�LB	�RB	�XB	�^B	�dB	�qB	�wB	�}B	��B	��B	B	ÖB	ĜB	ĜB	ŢB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�5B	�;B	�BB	�ZB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
MB
�B
'B
1�B
5�B
:�B
AB
HB
I�B
OBB
UMB
Y�B
^jB
d�B
lWB
tB
y>B
}�B
�i111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�/B�/B�)B�)B�)B�/B�5B�/B�/B�/B�rBȮB�"B��B:BXB_BqB�B-BL�BV�B\BeQBm�B{�B�B�&B�3B�DB�]B�JB�JB�PB�PB�WB�cB�iB�iB�oB�oB�iB�]B�?B�B��B�B{�B|�By�Bu�Bt�Bu�Bs�Bu�Bt�Bt�Bw�Bv�Bx�Bu�Bs�Bs�Br�Bs�Bq�Bn�Bk{Bk{BgcBf]Bf]BbEB`9BP�BN�BK�BI�B=kB.B�B�B�BlB
<B��B��B�+B��B��B�|B�>Bz�BejBWBP�BH�B-B�B�BB<B
�B
��B
�cB
��B
��B
�YB
�4B
z�B
r�B
j�B
``B
W)B
N�B
J�B
E�B
<�B
.7B
(B
�B
KB	��B	��B	�B	�B	��B	��B	��B	�MB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�sB	�6B	h�B	T,B	H�B	C�B	:�B	0WB	$B	�B	 :B��B�B�ZB�0B��B��B�yB�=B�1B�B�B��B��B��B��B�uB~9B|-Bv	Bk�Bh�Bf�Bd�Bd�B_�B_�BY_BVMBVMBUGBWSBWSBWSBWSBWSBWSBVMBUGBUHBVNBTBBS<BS<BMBKBJBF�BE�BD�BC�BB�BA�B?�B?�B<�B8�B8�B1vB/jB.dB-^B,XB,XB-^B+RB+RB*LB)GB*MB(AB)GB(AB(AB';B&5B$)B$)B$)B'<B'<B*NB)HB(BB)HB-`B+TB-`B/mB0tB0tB1zB1zB2�B3�B3�B3�B4�B5�B6�B:�B:�B;�B;�B:�B:�B;�B=�B@�BE�BLBM BM BM BM BN&BSEBVWBW]BYjBa�Be�Bk�Bk�Bk�Bl�Bl�Bn�Bo�Bo�Bl�Bk�Bn�Bo�Bq�By'B}@B�WB��B��B��B��B�B�3B�:B�:B�:B�?B�EB�KB�KB�QB�dB�pB�|B��B��B��B��B��B�!B�-B�4B�:B�WBނB�B�B�B��B��B��B��B�B� B�&B�&B�3B�?B�EB	QB	dB	vB	vB	
�B	�B	�B	�B	�B	�B	�B	�B	�B	#B	*EB	,QB	.^B	1oB	3{B	8�B	:�B	<�B	C�B	H�B	NB	P&B	R3B	S9B	T>B	T?B	VJB	VJB	WPB	ZcB	]uB	^{B	`�B	a�B	b�B	b�B	c�B	e�B	k�B	l�B	p�B	q�B	r�B	s�B	wB	z B	~9B	�iB	�oB	�oB	�iB	�iB	�iB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�3B	�3B	�3B	�9B	�?B	�DB	�PB	�]B	�cB	�iB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	� B	�'B	�'B	�'B	�'B	�-B	�-B	�2B	�DB	�JB	�QB	�iB	�uB	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	�B
ZB
�B
*B
)�B
.B
3B
9B
@"B
A�B
GMB
MXB
Q�B
VuB
]B
daB
l)B
qHB
u�B
xr111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.21 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144152022020411441520220204114415  AO  ARCAADJP                                                                    20200619170905    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170905  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170905  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114415  IP                  G�O�G�O�G�O�                