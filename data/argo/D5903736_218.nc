CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-05-14T17:02:19Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20180514170219  20190604094145  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�`
��V01   @�`���@6cS����d{t�j~�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@�  @���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtY�Dy��D�qD�P�D�|)D���D�	�D�2�D���D��3D�qD�N�D�s�D���D� D�6�Dڌ{D��\D��D�(�D�x�D�q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @,(�@r�\@�{@�{A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/(�B7�\B?(�BFBO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C��C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddl)Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dix�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�DtL)Dy�\D���D�I�D�uqD�� D�4D�+�D���D��{D��D�H D�l�D��D�	HD�0 Dڅ�D���D�D�!�D�q�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#A���A���A�ĜA�ȴA���A��A���A�  A�A�?}A��mA�A��#AƾwAƲ-AƋDA�~�A�t�A�l�A�l�Aƙ�AƗ�A�~�A�G�AÃA¶FA��A���A�l�A��mA�l�A��A�dZA�$�A���A�ƨA���A��wA�VA���A���A��/A��HA�C�A�oA�v�A�G�A��#A�bNA�ĜA�ƨA�(�A��TA���A�A�A��`A�{A��A��A�JA�M�A��A�x�A��7A�C�A��A�t�A��DA�A���A�^5A��\A��A���A��A�hsA���A��jA��A�x�A�1A�VA�G�A��A�;dA��hA�O�A��mA�O�A��A�VA��#A���A���A���A��A���A�
=A��A�^5A���A�l�A�\)A�5?A�"�A}�AzȴAw��Av-AtffAr�!Ap�Ao�7An�/Am�Al~�Ak�;Aj�9AiXAhz�Ag\)AfVAe;dAdv�Ab��Aa|�A`�+A^I�A[��AZ~�AY�AW��AVjAU�mAT�jAQ��APz�AO�AL��AKXAKVAIhsAGAFE�AC��AB�jAA�^AA+A>��A=�mA;��A9��A8r�A6�A6$�A5��A4^5A3hsA2ĜA2��A2�!A2��A1�A0�A.�RA-dZA+|�A*{A)7LA(bNA'`BA&�+A%\)A$��A$5?A#�^A"�A ��A �AoA=qAXA�A�hA?}A7LA�A�HAZAG�At�A��A�TAVA�A�hA�FA�uA9XA�^AVA�\A
A�A	|�A	`BA	"�A��A"�AI�AXA"�AffA|�A;dA+AVAȴAXA �yA ��A ĜA �A I�@��@�?}@���@��@���@���@���@�/@�t�@�@� �@���@�33@�\@��@�5?@陚@�A�@�!@�@�b@�@�-@��@߾w@�33@��@��@��@�v�@�5?@���@���@Չ7@��`@�Z@ӝ�@�^5@�-@ёh@��@�V@У�@�b@υ@�$�@��@��@��@�M�@��@�9X@��
@�|�@�^5@�/@Ĵ9@�A�@�
=@��-@�j@�9X@�b@�ƨ@�K�@�"�@�^5@�p�@�V@�A�@��@���@���@�X@��@�t�@��@���@�@�^5@�E�@�-@�$�@�{@��@���@��-@��h@�p�@�hs@�hs@�&�@��@�Z@��m@��P@��+@��@��@�V@�Z@��@���@�l�@��@�-@��-@��@�z�@�9X@���@���@���@�n�@�5?@��#@��7@�O�@�/@��j@�A�@� �@�1@��
@��F@��F@��P@�;d@��@���@�M�@��-@�%@���@��@�I�@�1'@�b@��
@��@��@�S�@�"�@�o@�@�ff@��7@��@��j@��u@�bN@�b@�ƨ@�\)@�"�@���@�{@��@��^@��7@�?}@�%@��u@���@��@��w@�l�@�K�@�;d@��@���@�{@��@�@��h@�?}@�V@��/@��u@�A�@���@���@��@�l�@�\)@�@���@��\@�v�@�^5@�5?@��T@���@�@���@���@�x�@�G�@�V@���@��j@��D@�Q�@�(�@�b@�  @��F@�t�@�;d@��H@�~�@���@���@��-@���@�`B@�/@���@��`@���@�r�@�Z@�b@��m@��@�t�@�C�@�;d@���@���@��R@���@��\@��\@���@��\@�n�@�M�@�5?@��@���@���@��-@�G�@��`@��@�Q�@�b@��@��;@���@���@�|�@�t�@�\)@�C�@��@���@�~�@�M�@�5?@�$�@�$�@�$�@�@��)@��E@|%�@v;�@j��@`�`@Y�S@T��@O�&@G�@?;d@76z@0�@*�F@#��@ ��@D�@��@X@�9@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��#A���A���A�ĜA�ȴA���A��A���A�  A�A�?}A��mA�A��#AƾwAƲ-AƋDA�~�A�t�A�l�A�l�Aƙ�AƗ�A�~�A�G�AÃA¶FA��A���A�l�A��mA�l�A��A�dZA�$�A���A�ƨA���A��wA�VA���A���A��/A��HA�C�A�oA�v�A�G�A��#A�bNA�ĜA�ƨA�(�A��TA���A�A�A��`A�{A��A��A�JA�M�A��A�x�A��7A�C�A��A�t�A��DA�A���A�^5A��\A��A���A��A�hsA���A��jA��A�x�A�1A�VA�G�A��A�;dA��hA�O�A��mA�O�A��A�VA��#A���A���A���A��A���A�
=A��A�^5A���A�l�A�\)A�5?A�"�A}�AzȴAw��Av-AtffAr�!Ap�Ao�7An�/Am�Al~�Ak�;Aj�9AiXAhz�Ag\)AfVAe;dAdv�Ab��Aa|�A`�+A^I�A[��AZ~�AY�AW��AVjAU�mAT�jAQ��APz�AO�AL��AKXAKVAIhsAGAFE�AC��AB�jAA�^AA+A>��A=�mA;��A9��A8r�A6�A6$�A5��A4^5A3hsA2ĜA2��A2�!A2��A1�A0�A.�RA-dZA+|�A*{A)7LA(bNA'`BA&�+A%\)A$��A$5?A#�^A"�A ��A �AoA=qAXA�A�hA?}A7LA�A�HAZAG�At�A��A�TAVA�A�hA�FA�uA9XA�^AVA�\A
A�A	|�A	`BA	"�A��A"�AI�AXA"�AffA|�A;dA+AVAȴAXA �yA ��A ĜA �A I�@��@�?}@���@��@���@���@���@�/@�t�@�@� �@���@�33@�\@��@�5?@陚@�A�@�!@�@�b@�@�-@��@߾w@�33@��@��@��@�v�@�5?@���@���@Չ7@��`@�Z@ӝ�@�^5@�-@ёh@��@�V@У�@�b@υ@�$�@��@��@��@�M�@��@�9X@��
@�|�@�^5@�/@Ĵ9@�A�@�
=@��-@�j@�9X@�b@�ƨ@�K�@�"�@�^5@�p�@�V@�A�@��@���@���@�X@��@�t�@��@���@�@�^5@�E�@�-@�$�@�{@��@���@��-@��h@�p�@�hs@�hs@�&�@��@�Z@��m@��P@��+@��@��@�V@�Z@��@���@�l�@��@�-@��-@��@�z�@�9X@���@���@���@�n�@�5?@��#@��7@�O�@�/@��j@�A�@� �@�1@��
@��F@��F@��P@�;d@��@���@�M�@��-@�%@���@��@�I�@�1'@�b@��
@��@��@�S�@�"�@�o@�@�ff@��7@��@��j@��u@�bN@�b@�ƨ@�\)@�"�@���@�{@��@��^@��7@�?}@�%@��u@���@��@��w@�l�@�K�@�;d@��@���@�{@��@�@��h@�?}@�V@��/@��u@�A�@���@���@��@�l�@�\)@�@���@��\@�v�@�^5@�5?@��T@���@�@���@���@�x�@�G�@�V@���@��j@��D@�Q�@�(�@�b@�  @��F@�t�@�;d@��H@�~�@���@���@��-@���@�`B@�/@���@��`@���@�r�@�Z@�b@��m@��@�t�@�C�@�;d@���@���@��R@���@��\@��\@���@��\@�n�@�M�@�5?@��@���@���@��-@�G�@��`@��@�Q�@�b@��@��;@���@���@�|�@�t�@�\)@�C�@��@���@�~�@�M�@�5?@�$�@�$�@�$�G�O�@��)@��E@|%�@v;�@j��@`�`@Y�S@T��@O�&@G�@?;d@76z@0�@*�F@#��@ ��@D�@��@X@�9@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
p�B
o�B
n�B
n�B
n�B
n�B
r�B
v�B
x�B
z�B
�uB
��B
�B
�B
�B
�B
�mB
�`B
�ZB
�TB
�mB
��B
��B
��BG�B�^B�TB��B%BPBoB�B)�B>wBF�BS�BcTBdZBn�Bq�Bv�By�B{�B{�Bz�B� B�B�B�B�B�Bz�Bz�B� B� B}�By�Bt�Bm�BgmBjBS�BI�BG�BF�B(�BbB��B�B�)B��B�B��B��B��Bw�Bm�Bl�BdZBVBQ�BL�BC�B9XB2-B-B$�BDBB
��B
�HB
��B
ǮB
B
�jB
�RB
�9B
�!B
��B
��B
� B
m�B
hsB
gmB
dZB
ZB
J�B
:^B
)�B
�B
uB
	7B	��B	��B	�B	�sB	�ZB	�5B	�B	��B	ŢB	��B	�^B	�9B	�B	��B	��B	�oB	�%B	v�B	n�B	ffB	\)B	VB	T�B	R�B	E�B	=qB	6FB	(�B	"�B	�B	�B	VB	%B��B��B�B�B�ZB�/B��B��BǮBB��B�wB�qB�jB�dB�dB�^B�XB�FB�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�bB�PB�JB�JB�DB�DB�7B�1B�+B�B�B�B� B~�B{�By�Bw�Bu�Bt�Br�Bo�Bl�BjBiyBgmBgmBe`Be`BffBe`Be`Be`Be`BdZBdZBcTBcTBdZBcTBcTBcTBbNBbNBdZBffBffBgmBgmBhsBiyBk�Bn�Bn�Bn�Bn�Bn�Bo�Bt�Bt�Bw�By�Bz�B|�B|�B~�B�B�B�+B�7B�=B�DB�VB�VB�\B�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�3B�3B�?B�RB�jB�wB��BƨB��B��B��B��B��B��B�
B�)B�5B�NB�fB�B�B�B�yB�sB�B�B��B��B��B��B��B��B��B��B��B��B	  B	B	B	B	B		7B	JB	\B	�B	�B	�B	�B	#�B	%�B	&�B	&�B	&�B	'�B	'�B	&�B	&�B	&�B	&�B	(�B	)�B	+B	,B	-B	.B	0!B	0!B	33B	7LB	9XB	9XB	:^B	;dB	;dB	<jB	?}B	B�B	D�B	F�B	J�B	P�B	S�B	T�B	W
B	XB	YB	[#B	\)B	\)B	^5B	`BB	`BB	`BB	cTB	hsB	l�B	n�B	p�B	q�B	s�B	u�B	y�B	z�B	}�B	�B	�B	�+B	�1B	�DB	�JB	�\B	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�3B	�3B	�?B	�RB	�RB	�XB	�XB	�^B	�jB	�qB	�qB	�wB	�wB	�}B	��B	��B	B	B	ĜB	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�#B	�)B	�/B	�;B	�;B	�HB	�NB	�TB	�TB	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
[B
PB
�B
)yB
1�B
<�B
C{B
F�B
I�B
O�B
RTB
Z�B
`B
e,B
j�B
m�B
q�B
t�B
xlB
|�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
`�B
_�B
^�B
^�B
^�B
^�B
b�B
f�B
h�B
j�B
�aB
��B
�B
��B
�xB
�tB
�NB
�AB
�;B
�9B
�PB
�B
��B
��B7�B�$B�B�B��B�B+B	YB�B.0B6aBC�BSBTB^NBacBfxBi�Bk�Bk�Bj�Bo�Bp�Bs�Bt�Br�Bs�Bj�Bj�Bo�Bo�Bm�Bi�BduB]NBW#BZ9BC�B9uB7lB6gB�B #B��B�{B��B��B��B��B��B�fBg�B]lB\iBT>BE�BA�B<�B3{B)>B"B�B�B
�3B
��B
�B
�:B
��B
��B
��B
�fB
�LB
�3B
�B
��B
��B
pB
]�B
XB
WxB
TbB
J%B
:�B
*lB
B
�B
�B	�MB	�B	��B	��B	؍B	�uB	�RB	�$B	��B	��B	��B	��B	�^B	�?B	�B	��B	��B	vQB	f�B	^�B	V�B	LXB	F3B	E/B	C'B	5�B	-�B	&�B	2B	B	�B	�B��B�fB�B��B��B��BԞB�yB�EB�B��B��B��B��B��B��B��B��B��B��B��B�{B�`B�LB�5B�+B�B�B�B�B��B��B��B��B��B��B��B��B��B��B}�B|�B|�B{�B{�By�Bx�Bw�Bu{BskBqcBp[BoUBlBBj7Bh*Bf BeBcB` B\�BZ�BY�BW�BW�BU�BU�BV�BU�BU�BU�BU�BT�BT�BS�BS�BT�BS�BS�BS�BR�BR�BT�BV�BV�BW�BW�BX�BY�B[�B^�B^�B^�B^�B^�B`BeBe#Bh2Bj=BkBBmOBmQBo^BrpBu�Bw�By�Bz�B{�B~�B~�B�B��B��B��B��B��B��B��B�B�B�B�	B�B�B�%B�0B�EB�VB�dB�|B��B��B��B��B��B��B��B��B�B�%B�+B�1B�:B�HB�QB�gB̇BΓBҫB��B��B��B��B��B��B��B��B�%B�HB�MB�LB�LB�JB�UB�VB�TB�XB�]B�_B�kB�tB�yB��B��B��B	�B	�B	
�B	B	-B	;B	AB	BB	?B	FB	BB	BB	BB	CB	?B	OB	OB	WB	]B	cB	kB	 wB	 vB	#�B	'�B	)�B	)�B	*�B	+�B	+�B	,�B	/�B	2�B	4�B	6�B	;B	A5B	DJB	EOB	G[B	HbB	IiB	KxB	L{B	LzB	N�B	P�B	P�B	P�B	S�B	X�B	\�B	^�B	`�B	a�B	dB	fB	j,B	k+B	nEB	tcB	ujB	wvB	x|B	{�B	|�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�(B	�6B	�<B	�AB	�RB	�YB	�pB	�tB	�vB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�,B	�3B	�1B	�6B	�QB	�OB	�VB	�ZB	�lB	�kB	�vB	πB	πB	ъB	ґB	ӒB	ӖB	դB	֪B	׮B	ײB	طB	طB	عB	ظB	شB	ٺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	� B	�B	�(B	�(B	�(B	�)B	�)G�O�B	�B	��B
	B
�B
!�B
,�B
3�B
7B
:B
@ B
B�B
J�B
PJB
UgB
[
B
^ B
a�B
d�B
h�B
l�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.21 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.016(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941452019060409414520190604094145  AO  ARCAADJP                                                                    20180514170219    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180514170219  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180514170219  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094145  IP                  G�O�G�O�G�O�                