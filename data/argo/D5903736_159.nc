CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:52Z creation      
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041152  20190604094023  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�ȯU��a1   @�ȯ�Яf@3M����dsI�^1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyVfD��D�H�D���D�ٚD��D�4)D�{�D��\D��D�B�D�h�D�ٚD��D�:�Dڊ�D�ҏD��D�C�D�)D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @x��@�G�@�G�A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B�ǮB��{B�aHC�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%�=C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[�=C]�=C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D ��Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#r�D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�DtX�DyH�D�4D�B>D�z>D���D��D�-qD�t�D�ؤD���D�;�D�b>D���D�>D�3�Dڄ)D���D�
>D�<�D�qD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�!A�-A�RA�^A�jA�jA�wA���A���A���A���A���A���A�A�A�jA�^A�!A�A�x�A�ĜA���AضFAו�A��mA��A�M�A�dZA�/A�$�A�r�A���A҅A�jA��A�C�A���A�?}AσA�ZA�33A̍PA�ZA˴9A�&�Aɴ9A���AŶFA�ȴA� �Aå�A�9XA��A��A�M�A�9XA�/A�t�A�oA��RA�p�A�+A���A�VA��A�z�A�ȴA�S�A�{A��wA�"�A�9XA��A�O�A�VA��A�VA���A��A�Q�A���A�C�A���A��wA���A��A�G�A�I�A���A�t�A��DA���A��hA��/A�bA���A�9XA�z�A�VA�5?A�$�A���A�C�A��hA�5?A��mA��
A��A��yA�A���A�v�A�
=A�|�A���A�S�A�G�A���A~�/A|^5Az{AxJAt�jAodZAl(�Aj�!Ag��Ae��AcoA`ffA^Q�A\�A[VAYAW��AW&�AV~�AV(�AU�-AT��AQ��AO�AL�jAK/AJz�AJ(�AI�;AI��AH��AGAE��AD��ADE�AC�TACdZA@��A>��A=hsA<�A:�A9
=A6��A4�9A3|�A1�#A0r�A.��A-�FA,��A,I�A+\)A*r�A)�^A(~�A'�
A&��A%x�A$�RA#`BA �A�AZA1AK�AoAAA�AG�AjAG�AffA�/A&�A5?A�FAȴA$�A��A�AhsA
�A	��A	G�A	�AQ�A�Az�A�mAĜA�A��A�A 1'@�~�@�`B@�A�@�+@�J@�G�@��D@�A�@�"�@�^5@��^@�hs@��@�F@��@�?}@�M�@�V@�9@��m@��@��@�@�F@�l�@��
@�u@�(�@���@��#@�&�@�I�@��@��@��@߶F@޸R@��T@�hs@��m@���@��@��m@֧�@֧�@�5?@Լj@�ȴ@�~�@�@��T@с@���@�Z@ϕ�@���@�V@̼j@� �@��m@��H@ț�@ȃ@�z�@���@�dZ@°!@���@��9@�(�@���@��D@���@�
=@���@�=q@�X@�x�@��@�`B@�G�@��j@��D@�z�@���@�+@��@�ff@���@���@�G�@��@� �@��;@�t�@���@�G�@�?}@���@�bN@�9X@�C�@�"�@���@��-@�Ĝ@���@�(�@�9X@�I�@� �@�@��h@�/@���@���@�z�@��m@��@��+@�V@��@�hs@��@�z�@�  @���@��@�l�@�;d@���@�@�X@�Ĝ@�(�@���@�S�@���@��@�ff@�$�@���@��-@���@�`B@��@��@��@��@�j@�Q�@�9X@��m@�S�@�;d@�"�@��@��H@��\@�{@��#@���@��7@�7L@���@��j@�j@�I�@�1@�ƨ@��@���@�;d@��y@���@�~�@�E�@�=q@��@��^@���@���@��@�O�@�&�@��`@��@�I�@�1@���@�\)@�+@�
=@�
=@�@���@�~�@�ff@�^5@�=q@�@�@��@�/@��`@��@�I�@�A�@�(�@��@�\)@�;d@�
=@�"�@�o@�~�@�^5@�M�@�M�@�V@�M�@��@��7@���@�I�@��@���@���@��
@��F@���@��@���@���@���@��@��H@���@���@��+@���@��j@��/@�x�@�%@���@���@�j@���@��@�O�@���@��/@���@��u@�r�@�Z@�Q�@�I�@�9X@� �@�1@��@�l�@�S�@�"�@���@��R@���@�V@�{@��T@�7L@��j@���@��@��@v)�@kt�@dɆ@]�@V��@PQ�@H~(@A!�@9o @49X@.ں@+;d@%u�@ �@b�@J�@��@1'@	0�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�!A�-A�RA�^A�jA�jA�wA���A���A���A���A���A���A�A�A�jA�^A�!A�A�x�A�ĜA���AضFAו�A��mA��A�M�A�dZA�/A�$�A�r�A���A҅A�jA��A�C�A���A�?}AσA�ZA�33A̍PA�ZA˴9A�&�Aɴ9A���AŶFA�ȴA� �Aå�A�9XA��A��A�M�A�9XA�/A�t�A�oA��RA�p�A�+A���A�VA��A�z�A�ȴA�S�A�{A��wA�"�A�9XA��A�O�A�VA��A�VA���A��A�Q�A���A�C�A���A��wA���A��A�G�A�I�A���A�t�A��DA���A��hA��/A�bA���A�9XA�z�A�VA�5?A�$�A���A�C�A��hA�5?A��mA��
A��A��yA�A���A�v�A�
=A�|�A���A�S�A�G�A���A~�/A|^5Az{AxJAt�jAodZAl(�Aj�!Ag��Ae��AcoA`ffA^Q�A\�A[VAYAW��AW&�AV~�AV(�AU�-AT��AQ��AO�AL�jAK/AJz�AJ(�AI�;AI��AH��AGAE��AD��ADE�AC�TACdZA@��A>��A=hsA<�A:�A9
=A6��A4�9A3|�A1�#A0r�A.��A-�FA,��A,I�A+\)A*r�A)�^A(~�A'�
A&��A%x�A$�RA#`BA �A�AZA1AK�AoAAA�AG�AjAG�AffA�/A&�A5?A�FAȴA$�A��A�AhsA
�A	��A	G�A	�AQ�A�Az�A�mAĜA�A��A�A 1'@�~�@�`B@�A�@�+@�J@�G�@��D@�A�@�"�@�^5@��^@�hs@��@�F@��@�?}@�M�@�V@�9@��m@��@��@�@�F@�l�@��
@�u@�(�@���@��#@�&�@�I�@��@��@��@߶F@޸R@��T@�hs@��m@���@��@��m@֧�@֧�@�5?@Լj@�ȴ@�~�@�@��T@с@���@�Z@ϕ�@���@�V@̼j@� �@��m@��H@ț�@ȃ@�z�@���@�dZ@°!@���@��9@�(�@���@��D@���@�
=@���@�=q@�X@�x�@��@�`B@�G�@��j@��D@�z�@���@�+@��@�ff@���@���@�G�@��@� �@��;@�t�@���@�G�@�?}@���@�bN@�9X@�C�@�"�@���@��-@�Ĝ@���@�(�@�9X@�I�@� �@�@��h@�/@���@���@�z�@��m@��@��+@�V@��@�hs@��@�z�@�  @���@��@�l�@�;d@���@�@�X@�Ĝ@�(�@���@�S�@���@��@�ff@�$�@���@��-@���@�`B@��@��@��@��@�j@�Q�@�9X@��m@�S�@�;d@�"�@��@��H@��\@�{@��#@���@��7@�7L@���@��j@�j@�I�@�1@�ƨ@��@���@�;d@��y@���@�~�@�E�@�=q@��@��^@���@���@��@�O�@�&�@��`@��@�I�@�1@���@�\)@�+@�
=@�
=@�@���@�~�@�ff@�^5@�=q@�@�@��@�/@��`@��@�I�@�A�@�(�@��@�\)@�;d@�
=@�"�@�o@�~�@�^5@�M�@�M�@�V@�M�@��@��7@���@�I�@��@���@���@��
@��F@���@��@���@���@���@��@��H@���@���@��+@���@��j@��/@�x�@�%@���@���@�j@���@��@�O�@���@��/@���@��u@�r�@�Z@�Q�@�I�@�9X@� �@�1@��@�l�@�S�@�"�@���@��R@���@�V@�{@��T@�7L@��jG�O�@��@��@v)�@kt�@dɆ@]�@V��@PQ�@H~(@A!�@9o @49X@.ں@+;d@%u�@ �@b�@J�@��@1'@	0�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB?}B?}B?}B?}B?}B?}B@�B@�BC�B,BB
�#B
�)B
�;B
�B�B?}B49B"�B�B)�B;dB@�BI�BZBiyBp�Bu�B�B�'B��B�TB�yB�`B�BR�Bu�B�1B�%B� B�B� B}�B~�B�bB��B��B��B��B��B��B��B�wBĜB�B��B�)B�mB�B�B�5BǮB�B��B�!B�B��B��B��B��B��B��B�VB�B}�Bu�B`BBD�B@�B8RB,B"�BJB��B�B�yB�HB��B�}B�B��B�Bs�BR�B<jB+B�B
��B
�B
�yB
�mB
�HB
�B
��B
ǮB
ÖB
�B
�VB
z�B
e`B
T�B
5?B
B	�fB	�5B	��B	�?B	��B	��B	�+B	|�B	s�B	iyB	cTB	`BB	\)B	ZB	T�B	N�B	?}B	5?B	+B	%�B	#�B	!�B	 �B	�B	�B	bB		7B	B	B��B��B�B�B�fB�HB�#B��BɺB��B�XB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B�bB�7B�B�B�B|�Bv�Br�Bo�Bl�BiyBdZBbNB^5B\)B\)B[#B\)B\)B[#B[#B\)BW
BZB^5BbNBgmBhsBgmBffBdZBaHB[#BXBXBZBbNBgmBk�BiyBhsBgmBffBjBo�Bq�Br�By�B�B�1B�1B�B~�B�B�B�%B�7B�PB�\B�oB��B��B��B�B�B�B�B�B�B�9B�9B�?B�FB�FB�LB�RB�RB�FB�LB�XB�^B�FB�9B�}BBƨBȴBɺBɺB��B��B��B��B��B��B��BɺBȴBǮB��B�^B�RB�FB�dB�wBȴB��B��B�B�BB�ZB�mB�sB�B�B�B��B��B��B��B��B��B��B	  B	B	%B	B	B	B	+B	1B	JB	JB	hB	{B	�B	�B	�B	�B	�B	 �B	 �B	'�B	)�B	.B	2-B	0!B	0!B	1'B	5?B	;dB	=qB	?}B	?}B	A�B	D�B	F�B	J�B	N�B	P�B	R�B	T�B	T�B	T�B	XB	YB	\)B	aHB	cTB	e`B	hsB	jB	l�B	m�B	p�B	r�B	t�B	u�B	v�B	x�B	{�B	}�B	� B	�B	�%B	�1B	�7B	�DB	�bB	�bB	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�3B	�3B	�RB	�^B	�^B	�^B	�dB	�jB	�qB	�}B	��B	B	ÖB	ŢB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�;B	�;B	�;B	�;B	�HB	�TB	�ZB	�ZB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
zB
�B
 'B
)yB
3�B
:B
A�B
F�B
L�B
T{B
Z7B
^�B
b�B
c�B
iyB
mwB
r�B
v�B
|jB
�UB
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B;�B;�B;�B;�B;�B;�B;�B;�B;�B;�B;�B<�B<�B<�B<�B<�B<�B=�B=�B@�B)MB UB
�lB
�nB
܀B
��B�B<�B1}B B�B'@B8�B=�BF�BWcBf�Bm�BsB�\B�dB��B��B�B�B��BP1Br�B�qB�cB}>B~CB}5B{0B|<B��B��B�B�)B�(B�%B�B��B��B��B�PB��B�gB�B��B��B�yB��B�IB�(B�cB�DB�B�B�B�B��B��B��B�cB{5BsB]�BA�B=�B5�B)MB B	�B�B��B�BސB� B��B�HB��B�^Bp�BP=B9�B(KB�B
�CB
��B
��B
�B
ޓB
�]B
�B
��B
��B
�LB
��B
x2B
b�B
RQB
2�B	�YB	�B	ۋB	�B	��B	�6B	��B	��B	zGB	qB	f�B	`�B	]�B	Y�B	WuB	RVB	L0B	<�B	2�B	(ZB	#?B	!5B	(B	#B	B	�B	�B	�B	tB�hB�PB�-B��B��B��BޣB؁B�OB�B��B��B��B�uB�VB�<B�+B�1B�3B�>B�+B�B�B�B��B��B��B��B�zBlB~hBzQBt'BpBmBi�Bf�Ba�B_�B[�BY�BY�BX�BY�BY�BX�BX�BY�BTnBW�B[�B_�Bd�Be�Bd�Bc�Ba�B^�BX�BUsBUuBW�B_�Bd�Bh�Bf�Be�Bd�Bc�Bg�BmBoBpBw=B��B��B��BqB|]BtB�uB��B��B��B��B��B��B�`B�\B�oB�~B�yB�~B�|B�yB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�2B�8B�FB�FB�BB�,B�B�B�B��B��B��B��B��B��B�B�>B�GB�BݦB��B��B��B��B��B��B�B�$B�(B�,B�0B�9B�LB�`B	~B	�B	|B	 zB	}B	�B	�B		�B		�B	�B	�B	�B	�B	B	B	B	&B	'B	%TB	'_B	+xB	/�B	-�B	-�B	.�B	2�B	8�B	:�B	<�B	<�B	>�B	A�B	DB	HB	L;B	NHB	PTB	RcB	R^B	R]B	UoB	V{B	Y�B	^�B	`�B	b�B	e�B	g�B	i�B	j�B	nB	pB	r B	s$B	t+B	v9B	yHB	{WB	}bB	jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�*B	�0B	�?B	�FB	�IB	�PB	�VB	�WB	�jB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�1B	�1B	�1B	�8B	�5B	�6B	�FB	�QB	�`B	�kB	�{B	�tB	�{B	�tB	�vB	�~B	ٍB	ڐB	ڒB	ڏB	ڏB	ڐB	ڐB	ڎB	ܘB	ܚB	ܚB	ܛB	ޫB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�$B	�/B	�.B	�6B	�8B	�:B	�IB	�PB	�QB	�QB	�QB	�RB	�TB	�aB	�gB	�eB	�lB
 pB
vB
xB
wB
{B
 rB	�kB	�eG�O�B
�B
B
�B
&�B
0�B
7lB
?7B
DB
JB
Q�B
W�B
\NB
`B
aRB
f�B
j�B
pB
t\B
y�B
~�B
�x11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.21 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.003(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940232019060409402320190604094023  AO  ARCAADJP                                                                    20181121041152    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041152  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041152  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094023  IP                  G�O�G�O�G�O�                