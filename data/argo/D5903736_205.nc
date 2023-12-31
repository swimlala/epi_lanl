CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-12-22T01:04:20Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20171222010420  20190604094031  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�>�E�2)1   @�>�nz�@4�;dZ��d�ě��T1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Dy�=D� D�J�D�mqD���D�fD�ND���D���D���D�@�D�e�DǨ D�\D�=�D�=D���D��D�A�D��D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @x��@�G�@�G�A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B�aHB��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�Dy��D�	HD�D)D�f�D��D��D�G\D�z>D��>D��D�9�D�_DǡHD��D�7D��D��D��4D�;4D�4D�Å111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bA�VA�bA�oA�oA�bA�VA�VA�
=A�
=A�
=A�
=A�JA�JA�JA�JA�JA�JA�
=A�1A�
=A�
=A�JA�1A���A���A��A��mA��
A�ƨA���AŶFA�;dA�G�A�z�A���A���A�&�A��A��7A�x�A�1'A��A���A��yA�ĜA���A��PA�r�A�5?A�p�A�ĜA�l�A�bA�r�A�/A�"�A���A�O�A��-A�A��/A�S�A��/A��A��HA���A��A�I�A��;A���A���A��PA�A��!A�+A�?}A��;A���A�ZA��!A��\A�v�A�=qA��A��\A���A�JA��FA�+A�A�A�Q�A��\A��PA���A�A�n�A��9A���A�9XA�%AdZA}�
Az��Ay��AvjAt�+As�TAq�
Aq�Ap�An1Am|�Am33Al�+AkK�Aj{Ah1'Af��AfQ�Ae�PAe�Ad��Ac��Aa�A_��A]33AZ�AXE�AV{AS��AQ��AP�uAOx�AM+AKC�AJ�AH�yAG��AG%AFv�AEG�AD{AB��AA��AA
=A@ �A?�A?A>ffA=��A=K�A=�A<�!A;�TA:�/A: �A7ƨA6ȴA5�PA4�DA3hsA1�A0�A0  A/;dA.�A-�7A,�A*Q�A&��A#�PA!|�A �!A (�AdZA�A33AhsA�A5?A��AZA�mA�A��A��A�mA33A�A�AI�A�FA�/A$�A��AȴA33A
(�AĜA5?A�A�^A|�AXAK�A
=A�HA��A��Az�AE�A�AO�Ar�AĜA\)@�dZ@�ff@�5?@�G�@�r�@��@��h@�R@�=q@�{@��@��@@�7@��@�bN@�9X@�  @�!@�u@��`@�C�@⟾@���@�b@߅@��@ޟ�@�{@ݺ^@�`B@�V@�z�@��;@ۅ@���@�;d@�~�@���@�hs@ץ�@��@�M�@�X@ԃ@�;d@���@�  @�@��y@�9X@�9X@���@ɑh@�&�@��@�@�;d@�`B@��@�z�@�9X@�1@�ƨ@�\)@��H@�~�@�{@�hs@�%@��`@���@�ƨ@�ff@�?}@��j@�j@�1@��y@�=q@�p�@��@�b@�-@�r�@�K�@�@���@�-@���@�hs@��@��@���@�j@�j@�j@�A�@�b@���@���@���@��@�7L@��@���@���@�Q�@���@�v�@�G�@���@�A�@���@���@���@���@���@��\@�~�@�n�@�ff@�n�@�M�@�5?@�@���@��h@�O�@���@���@���@�G�@�V@��@��9@��u@�j@�1'@��;@��;@��@�|�@�C�@�@��H@���@���@��\@�v�@�=q@��@��#@��^@���@�`B@�V@�r�@��9@��@��@��u@��u@��@�Z@��@�\)@�dZ@�t�@�t�@�;d@��y@�ȴ@���@�~�@�$�@�J@�{@�J@��@�X@�X@�`B@�p�@�G�@��@���@��j@��9@���@��@�S�@�"�@��R@�5?@��@���@���@��@�hs@��u@��w@�dZ@�S�@�33@�
=@��H@���@�v�@�$�@�@�`B@�&�@��@��@�V@��@���@�Ĝ@���@�z�@�Z@��@��
@��
@��w@�ƨ@��F@���@���@��@��@�M�@�-@�$�@�@��^@�hs@��`@�Q�@�(�@��@��
@��P@�C�@��@��!@��@��-@�x�@�O�@�O�@�7L@��@�V@���@�Ĝ@�z�@�Q�@�I�@�I�@�z�@��u@��@�z�@�r�@�z�@�z�@�I�@�A�@�A�@�(�@��@{�@v��@n��@h9X@a��@X��@K�@D�5@@�@:a|@5^�@0!@)Y�@"a|@�`@�e@��@�@�@?�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�bA�VA�bA�oA�oA�bA�VA�VA�
=A�
=A�
=A�
=A�JA�JA�JA�JA�JA�JA�
=A�1A�
=A�
=A�JA�1A���A���A��A��mA��
A�ƨA���AŶFA�;dA�G�A�z�A���A���A�&�A��A��7A�x�A�1'A��A���A��yA�ĜA���A��PA�r�A�5?A�p�A�ĜA�l�A�bA�r�A�/A�"�A���A�O�A��-A�A��/A�S�A��/A��A��HA���A��A�I�A��;A���A���A��PA�A��!A�+A�?}A��;A���A�ZA��!A��\A�v�A�=qA��A��\A���A�JA��FA�+A�A�A�Q�A��\A��PA���A�A�n�A��9A���A�9XA�%AdZA}�
Az��Ay��AvjAt�+As�TAq�
Aq�Ap�An1Am|�Am33Al�+AkK�Aj{Ah1'Af��AfQ�Ae�PAe�Ad��Ac��Aa�A_��A]33AZ�AXE�AV{AS��AQ��AP�uAOx�AM+AKC�AJ�AH�yAG��AG%AFv�AEG�AD{AB��AA��AA
=A@ �A?�A?A>ffA=��A=K�A=�A<�!A;�TA:�/A: �A7ƨA6ȴA5�PA4�DA3hsA1�A0�A0  A/;dA.�A-�7A,�A*Q�A&��A#�PA!|�A �!A (�AdZA�A33AhsA�A5?A��AZA�mA�A��A��A�mA33A�A�AI�A�FA�/A$�A��AȴA33A
(�AĜA5?A�A�^A|�AXAK�A
=A�HA��A��Az�AE�A�AO�Ar�AĜA\)@�dZ@�ff@�5?@�G�@�r�@��@��h@�R@�=q@�{@��@��@@�7@��@�bN@�9X@�  @�!@�u@��`@�C�@⟾@���@�b@߅@��@ޟ�@�{@ݺ^@�`B@�V@�z�@��;@ۅ@���@�;d@�~�@���@�hs@ץ�@��@�M�@�X@ԃ@�;d@���@�  @�@��y@�9X@�9X@���@ɑh@�&�@��@�@�;d@�`B@��@�z�@�9X@�1@�ƨ@�\)@��H@�~�@�{@�hs@�%@��`@���@�ƨ@�ff@�?}@��j@�j@�1@��y@�=q@�p�@��@�b@�-@�r�@�K�@�@���@�-@���@�hs@��@��@���@�j@�j@�j@�A�@�b@���@���@���@��@�7L@��@���@���@�Q�@���@�v�@�G�@���@�A�@���@���@���@���@���@��\@�~�@�n�@�ff@�n�@�M�@�5?@�@���@��h@�O�@���@���@���@�G�@�V@��@��9@��u@�j@�1'@��;@��;@��@�|�@�C�@�@��H@���@���@��\@�v�@�=q@��@��#@��^@���@�`B@�V@�r�@��9@��@��@��u@��u@��@�Z@��@�\)@�dZ@�t�@�t�@�;d@��y@�ȴ@���@�~�@�$�@�J@�{@�J@��@�X@�X@�`B@�p�@�G�@��@���@��j@��9@���@��@�S�@�"�@��R@�5?@��@���@���@��@�hs@��u@��w@�dZ@�S�@�33@�
=@��H@���@�v�@�$�@�@�`B@�&�@��@��@�V@��@���@�Ĝ@���@�z�@�Z@��@��
@��
@��w@�ƨ@��F@���@���@��@��@�M�@�-@�$�@�@��^@�hs@��`@�Q�@�(�@��@��
@��P@�C�@��@��!@��@��-@�x�@�O�@�O�@�7L@��@�V@���@�Ĝ@�z�@�Q�@�I�@�I�@�z�@��u@��@�z�@�r�@�z�@�z�@�I�@�A�@�A�G�O�@��@{�@v��@n��@h9X@a��@X��@K�@D�5@@�@:a|@5^�@0!@)Y�@"a|@�`@�e@��@�@�@?�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB:^B:^B:^B9XB9XB:^B:^B:^B:^B:^B:^B:^B:^B;dB;dB;dB;dB;dB<jB<jB<jB<jB<jB<jB>wB?}BA�BB�BE�BG�BH�BH�BH�BF�B@�B7LB8RB6FB33B0!B2-B2-B49B7LB9XB;dB<jB=qB>wB@�BK�BO�BN�BN�BL�BL�BL�BP�BR�B\)BdZBaHBl�By�Bu�Bn�B_;B_;BT�BD�B�B	7B��B��B��B�B�ZBƨB�XB��B��B��B��B��B�7B� Bv�BgmBS�B>wB#�B�BB
�B
�
B
�wB
�B
��B
�=B
�B
}�B
v�B
hsB
S�B
H�B
8RB
,B
&�B
�B
�B
hB
1B
B
B	��B	��B	�B	�ZB	�)B	�B	��B	��B	��B	ŢB	�jB	�-B	�B	��B	�uB	�1B	{�B	p�B	hsB	`BB	VB	L�B	F�B	A�B	;dB	7LB	33B	-B	&�B	�B	�B	�B	hB	PB	
=B	+B	B	  B��B��B��B��B�B�sB�NB�5B�B��B��BɺBƨBÖB��B�qB�XB�3B��B��B�hB�PB�=B�1B�B�B� B}�B|�B{�Bz�By�Bx�Bu�Bs�Bq�Br�Br�Br�Bt�Br�Bt�Br�Bp�Bp�Bo�Bs�Bt�Bt�Bu�Bu�Bv�Bv�Bu�Bu�Bv�Bv�Bw�Bv�Bv�Bu�Bu�Bt�Bs�Bm�BcTB[#B[#BZBXBT�BN�BJ�BI�BJ�BJ�BI�BG�BD�BD�BE�BE�BD�BD�BF�BN�BS�BW
BaHBffBjBo�Bq�Bv�Bw�Bv�Bv�Bv�Bv�Bv�Bx�B� B�{B��B��B��B��B�{B�bB�JB�+B�B�B�%B�VB��B��B��B��B��B�B�B��B��B�B�B�B�!B�-B�9B�LB�XB�^B�qB�wB�wB�qB�wB��BBÖBÖBĜBŢBŢBǮB��B��B��B�
B�/B�;B�HB�TB�TB�`B�fB�sB�B�B�B�B�B�B�B�B��B	JB	\B	uB	�B	�B	�B	�B	 �B	&�B	'�B	+B	0!B	6FB	8RB	8RB	9XB	:^B	;dB	=qB	A�B	D�B	F�B	H�B	K�B	M�B	N�B	O�B	Q�B	S�B	VB	_;B	bNB	gmB	iyB	jB	jB	l�B	p�B	q�B	q�B	r�B	t�B	w�B	x�B	z�B	|�B	}�B	� B	�B	�B	�=B	�PB	�\B	�hB	�{B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�-B	�9B	�?B	�?B	�FB	�LB	�dB	�qB	��B	B	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�/B	�/B	�5B	�;B	�5B	�5B	�;B	�5B	�;B	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
%B
+B
+B
+B
+B
	7B

=B
DB
PB
\B
bB
bB
bB
bB
hB
oB
uB
uB
{B
{B
�B
�B
*eB
1'B
7�B
<jB
DgB
MB
Q�B
W$B
[�B
`B
f�B
l�B
q�B
v�B
{0B
~B
�4B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B-B-B-B,B,	B-B-B-B-B-B-B-B-B.B.B.B.B.B/"B/B/B/B/B/B1)B20B4>B5BB8UB:aB;hB;hB;gB9XB36B*B+B(�B%�B"�B$�B$�B&�B*B,B.B/B0(B12B39B>{BB�BA�BA�B?�B?�B?�BC�BE�BN�BWBS�B_?Bl�BhyBaLBQ�BQ�BG�B7TBgB��B�B�B�B�{B�!B�sB�*B��B��B��B�xB�dB|
Br�Bi�BZBBF�B1WB�B	kB
��B
�dB
��B
�cB
��B
�B
}3B
uB
p�B
i�B
[jB
F�B
;�B
+NB
	B
�B
�B

�B
hB	�2B	�!B	�B	��B	��B	�B	�bB	�3B	�B	��B	��B	��B	��B	�wB	�9B	�B	��B	��B	{CB	n�B	c�B	[�B	SXB	IB	?�B	9�B	4�B	.�B	*gB	&QB	 ,B		B	�B	�B	�B	�B	 rB�cB�NB�)B�)B�"B�B�B��B��BۛB�uB�^B�?B�$B�B��B��B��B��B��B��B�cB� B��B��B��B}tB{fBxPBvIBs6Bq.Bp%Bo$BnBmBlBh�Bf�Bd�Be�Be�Be�Bg�Be�Bg�Be�Bc�Bc�Bb�Bf�Bg�Bg�Bh�BiBjBjBiBiBjBjBkBjBjBh�Bh�Bg�Bf�B`�BV�BNfBNeBM^BKUBH?BBB>B<�B>B>B= B:�B7�B7�B8�B8�B7�B7�B9�BBBG;BJNBT�BY�B]�Bb�Bd�BjBkBjBjBjBjBjBlBsBB��B��B��B��B��B��B��B�BzjBvTBvTBygB��B�B��B�B�1B�6B�TB�NB�<B�:B�?B�LB�QB�\B�nB�yB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�GB�lB�wB�}B֏B֎BؠBٝBۯB��B��B��B��B��B��B��B��B�)B��B	�B	�B	�B	
�B	�B	�B	�B	!B	-B	6B	#ZB	)}B	+�B	+�B	,�B	-�B	.�B	0�B	4�B	7�B	9�B	;�B	>�B	AB	BB	CB	EB	G+B	I:B	RlB	UB	Z�B	\�B	]�B	]�B	_�B	c�B	d�B	d�B	e�B	g�B	kB	l	B	nB	pB	q&B	s0B	v@B	xMB	}jB	��B	��B	��B	��B	��B	��B	�B	�/B	�-B	�/B	�/B	�0B	�*B	�9B	�SB	�[B	�YB	�ZB	�eB	�jB	�hB	�rB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�!B	�$B	�&B	�-B	�/B	�?B	�RB	�XB	�VB	�]B	�`B	�\B	�^B	�dB	�]B	�cB	�sB	�B	�zB	�xB	�B	؄B	ٌB	ٍB	َB	يB	ڔB	ۚB	ܟB	ݦB	ެB	߳B	߰B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�3B	�<B	�FB	�DB	�JB	�LB	�PB	�MB	�PB	�PB	�]B	�bB	�fB
 rB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
!B
�B
�B
$KB
*�B
/�B
7�B
@B
D�B
JAB
N�B
S/B
Y�B
`B
d�B
i�B
nOB
q-B
sTB
w9B
z2111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.21 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.013(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940312019060409403120190604094031  AO  ARCAADJP                                                                    20171222010420    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171222010420  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171222010420  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094031  IP                  G�O�G�O�G�O�                