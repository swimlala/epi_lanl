CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:51Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121041151  20190604094022  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @׻�@yn|1   @׻���(@3��`A��d3|�hs1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�\D�B=D�yHD���D��D�L�D��RD�D��D�D�D�x�D��
D��D�G�DڅD�ָD���D�<)D�t{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@�G�@�{A��A<��A\��A|��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B(�B(�B(�B'(�B/(�B7(�B?(�BG(�BO(�BW(�B_(�Bg(�Bo(�Bw(�B(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BÔ{Bǔ{B˔{Bϔ{BӔ{Bה{B۔{Bߔ{B�{B�{B�{B�{B�{B��{B��{B��{C�=C�=C�=C�=C	�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C�=C!�=C#�=C%��C'�=C)�=C+�=C-�=C/�=C1�=C3�=C5�=C7�=C9�=C;�=C=�=C?�=CA�=CC�=CE�=CG�=CI�=CK�=CM�=CO�=CQ�=CS�=CU�=CW�=CY�=C[��C]��C_�=Ca�=Cc�=Ce�=Cg�=Ci�=Ck�=Cm�=Co�=Cq�=Cs�=Cu�=Cw�=Cy�=C{�=C}�=C�=C��C��C��C��C��RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D r�D �Dr�D�Dr�D�Dr�D�Dr�D��Dr�D�Dr�D�Dr�D�Dr�D��D	r�D	�D
r�D
�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�Dr�D�D r�D �D!r�D!�D"r�D"�D#l)D#�D$r�D$�D%r�D%�D&r�D&�D'r�D'�D(r�D(�D)r�D)�D*r�D*�D+r�D+�D,r�D,�D-r�D-�D.r�D.�D/r�D/�D0r�D0�D1r�D1�D2r�D2�D3r�D3�D4r�D4�D5r�D5�D6r�D6�D7r�D7�D8r�D8�D9r�D9�D:r�D:�D;r�D;�D<r�D<�D=r�D=�D>r�D>�D?r�D?�D@r�D@�DAr�DA�DBr�DB�DCr�DC�DDr�DD�DEr�DE�DFr�DF�DGr�DG�DHr�DH�DIr�DI�DJr�DJ�DKr�DK�DLr�DL�DMr�DM�DNr�DN�DOr�DO�DPr�DP�DQr�DQ�DRr�DR�DSr�DS�DTr�DT�DUr�DU�DVr�DV�DWr�DW�DXr�DX�DYr�DY�DZr�DZ�D[r�D[�D\r�D\�D]r�D]�D^r�D^�D_r�D_�D`r�D`�Dar�Da�Dbr�Db�Dcr�Dc�Ddr�Dd�Der�De�Dfr�Df�Dgr�Dg�Dhr�Dh�Dir�Di�Djr�Dj�Dkr�Dk�Dlr�Dl�Dmr�Dm�Dnr�Dn�Dor�Do�Dpr�Dp�Dqr�Dq�Drr�Dr�Dsr�Ds�Dtr�Dt�\Dy�D��D�;�D�r�D��4D�>D�FD���D���D�>D�>D�r>D��RD�D�@�D�~gD�� D��D�5qD�m�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A۟�A۟�A۟�AۮAۮAۮA۬Aۧ�AۃA�\)A�?}A�-A��A���A��Aں^Aڛ�A�ZA�C�A�/A�$�A�oA�A��A��/A�ȴA�ƨA���Aٲ-A٣�AّhA�~�A�x�A�ffA�M�A���A��A��;A��#A�r�A�\)AѼjA��A�+A�=qAѾwAѓuA�M�AиRA���A�33A�Q�A�hsA�oA�ƨA�$�A��A�?}A���AʁA���A�O�A��#AȰ!A�oAǡ�A�7LA��HAƟ�Aũ�Aĕ�A�XAÛ�A���A��A�VA��yA��uA���A�9XA���A���A���A�=qA���A�ȴA�XA��9A��A��PA��A�9XA�+A���A�7LA���A���A��hA�p�A�XA���A�K�A��7A�=qA�5?A���A��-A���A��A�x�A�7LA��A���A��TA�/A��A��wA�A�-A�K�A��A~��A}�
A|=qA{�-A{�Ay�Awp�AtVAsl�Aq��Am�Ahz�Ac�A`�A_��A^�AZ�+AT�HAP�yAPn�AOXANJAL��AK��AG��AF�/AFE�AE�AE`BADABI�AAA?;dA>A�A=t�A;�A:M�A9�#A8��A7�A7+A5�hA4M�A3"�A2~�A1|�A09XA/�A.^5A,�`A+��A*v�A(n�A't�A'/A&M�A#�7A!A ȴA��A�DA��A7LA�A33AI�AS�A��A�jAjA1A�-AM�Ap�A��AȴA�A��A��A�TA�PA
9XA	��A	/AI�AZAA�A9XA(�A��A�AXA33A��A��AQ�AA �DA�A"�A�A��AȴAbA|�@��@�t�@��@��u@�z�@��9@�M�@���@��@��
@�o@��@��;@�(�@�b@� �@�Z@��@�R@�=q@�v�@�w@�j@���@�=q@�Ĝ@�~�@�-@��@�j@��@�%@�dZ@�n�@ى7@�I�@�\)@պ^@ԋD@�n�@��@У�@��;@�n�@�&�@�Z@���@ˍP@�M�@�7L@ȋD@ǶF@ǥ�@Ɨ�@�O�@�b@�n�@���@�p�@�V@��D@���@���@�=q@��#@�G�@��u@���@�"�@���@�E�@��@��
@���@�|�@�l�@�C�@�@���@�E�@���@�X@�&�@���@���@�C�@��\@�ff@�^5@�x�@��@��@��9@�bN@�I�@� �@���@�
=@��!@��T@�V@��@���@�1'@�ƨ@�t�@�"�@�@���@�V@�@���@�?}@�V@�r�@��F@�l�@�K�@�C�@�
=@���@�M�@���@�/@���@���@��u@�bN@�I�@�b@�;d@���@�V@�{@�x�@�r�@��@��@��w@�ȴ@�M�@�{@�5?@��h@��@��@��`@���@��@��9@�(�@� �@��@�S�@��@���@�ȴ@��@�@�=q@��@��T@���@���@���@�O�@��@���@��j@�r�@�1'@�  @���@���@��P@�S�@���@��-@��h@�hs@��j@�I�@�b@��w@���@�l�@�33@��@��H@��H@��H@��R@��R@��+@���@�@���@�x�@�hs@�p�@�x�@��@��@�n�@�v�@�E�@��7@�x�@�&�@��@���@��9@�Q�@� �@��
@���@��P@�dZ@�33@���@�=q@��@�@��@��#@��7@���@��u@�j@�(�@�ƨ@��F@�l�@�C�@�;d@�33@�+@��@�
=@���@�5?@�{@��@��T@���@�p�@�&�@��u@�I�@�  @��
@��F@��P@�l�@�o@��@��!@�=q@���@���@��h@�p�@�/@���@|��@u�M@k�@cg�@X�K@O��@F�b@@4n@9ϫ@3��@/��@)�@%S&@ ��@v`@4n@A�@��@>B@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A۟�A۟�A۟�AۮAۮAۮA۬Aۧ�AۃA�\)A�?}A�-A��A���A��Aں^Aڛ�A�ZA�C�A�/A�$�A�oA�A��A��/A�ȴA�ƨA���Aٲ-A٣�AّhA�~�A�x�A�ffA�M�A���A��A��;A��#A�r�A�\)AѼjA��A�+A�=qAѾwAѓuA�M�AиRA���A�33A�Q�A�hsA�oA�ƨA�$�A��A�?}A���AʁA���A�O�A��#AȰ!A�oAǡ�A�7LA��HAƟ�Aũ�Aĕ�A�XAÛ�A���A��A�VA��yA��uA���A�9XA���A���A���A�=qA���A�ȴA�XA��9A��A��PA��A�9XA�+A���A�7LA���A���A��hA�p�A�XA���A�K�A��7A�=qA�5?A���A��-A���A��A�x�A�7LA��A���A��TA�/A��A��wA�A�-A�K�A��A~��A}�
A|=qA{�-A{�Ay�Awp�AtVAsl�Aq��Am�Ahz�Ac�A`�A_��A^�AZ�+AT�HAP�yAPn�AOXANJAL��AK��AG��AF�/AFE�AE�AE`BADABI�AAA?;dA>A�A=t�A;�A:M�A9�#A8��A7�A7+A5�hA4M�A3"�A2~�A1|�A09XA/�A.^5A,�`A+��A*v�A(n�A't�A'/A&M�A#�7A!A ȴA��A�DA��A7LA�A33AI�AS�A��A�jAjA1A�-AM�Ap�A��AȴA�A��A��A�TA�PA
9XA	��A	/AI�AZAA�A9XA(�A��A�AXA33A��A��AQ�AA �DA�A"�A�A��AȴAbA|�@��@�t�@��@��u@�z�@��9@�M�@���@��@��
@�o@��@��;@�(�@�b@� �@�Z@��@�R@�=q@�v�@�w@�j@���@�=q@�Ĝ@�~�@�-@��@�j@��@�%@�dZ@�n�@ى7@�I�@�\)@պ^@ԋD@�n�@��@У�@��;@�n�@�&�@�Z@���@ˍP@�M�@�7L@ȋD@ǶF@ǥ�@Ɨ�@�O�@�b@�n�@���@�p�@�V@��D@���@���@�=q@��#@�G�@��u@���@�"�@���@�E�@��@��
@���@�|�@�l�@�C�@�@���@�E�@���@�X@�&�@���@���@�C�@��\@�ff@�^5@�x�@��@��@��9@�bN@�I�@� �@���@�
=@��!@��T@�V@��@���@�1'@�ƨ@�t�@�"�@�@���@�V@�@���@�?}@�V@�r�@��F@�l�@�K�@�C�@�
=@���@�M�@���@�/@���@���@��u@�bN@�I�@�b@�;d@���@�V@�{@�x�@�r�@��@��@��w@�ȴ@�M�@�{@�5?@��h@��@��@��`@���@��@��9@�(�@� �@��@�S�@��@���@�ȴ@��@�@�=q@��@��T@���@���@���@�O�@��@���@��j@�r�@�1'@�  @���@���@��P@�S�@���@��-@��h@�hs@��j@�I�@�b@��w@���@�l�@�33@��@��H@��H@��H@��R@��R@��+@���@�@���@�x�@�hs@�p�@�x�@��@��@�n�@�v�@�E�@��7@�x�@�&�@��@���@��9@�Q�@� �@��
@���@��P@�dZ@�33@���@�=q@��@�@��@��#@��7@���@��u@�j@�(�@�ƨ@��F@�l�@�C�@�;d@�33@�+@��@�
=@���@�5?@�{@��@��T@���@�p�@�&�@��u@�I�@�  @��
@��F@��P@�l�@�o@��@��!@�=q@���@���@��h@�p�G�O�@���@|��@u�M@k�@cg�@X�K@O��@F�b@@4n@9ϫ@3��@/��@)�@%S&@ ��@v`@4n@A�@��@>B@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
R�B
R�B
T�B
[#B
\)B
\)B
]/B
^5B
`BB
bNB
cTB
cTB
cTB
bNB
aHB
`BB
`BB
aHB
e`B
ffB
gmB
l�B
bNB
8RB
�B	��B
�B
>wB
l�B
�9B
ȴB
��B
�/B
�ZB
�HB
�B
�B
�B
��B
��BB
��BBDBuB�B+B9XBL�B[#BgmBt�B�B�JB�{B�3B��B�B�yB��BbB�B<jBE�BJ�BYB\)BgmB|�B�7B�uB��B�PB�VB� Bu�Bo�B`BBT�BffB��B��B�%B[#B8RBJBPB��B��BÖB��BiyBq�B�Bl�BVBE�B�B
��B
��B
��B
�mB
��B
ŢB
�?B
�JB
o�B
cTB
\)B
S�B
O�B
I�B
?}B
-B
�B
bB
B	�5B	�XB	��B	�B	x�B	k�B	Q�B	:^B	+B	'�B	"�B	�B	�B	hB	+B	B	B	  B��B��B�B�B�B�B�yB�fB�ZB�TB�HB�;B�/B�BB�BB�5B�5B�5B�NB�HB�5B�B�)B�B��B��B��B��B�wB�LB�FB�!B�B��B��B��B��B��B�{B�oB�uB�{B�oB��B�bB�B�B�B~�Bs�Bq�Bp�Bp�Bm�Bk�Bk�Bm�Bv�Bw�Bw�B|�B�JB�1B�=B�\B�=B|�Bz�Bx�Bt�B�1B��B��B��B��B��B��B�{B�DB�%B�B�B�%B�B� B|�By�B{�B�+B�=B�oB��B�PB�B|�Bz�Bz�B�=B�%B�PB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�9B�RB�RB�LB�^B�jB�qB�qB�}BBÖBÖBĜBŢB��B��B��B��B�B�)B�;B�NB�TB�TB�B�B�B�B�B��B��B��B��B��B��B	B	B	B		7B	JB	PB	\B	oB	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	(�B	/B	0!B	1'B	6FB	7LB	8RB	;dB	<jB	=qB	@�B	D�B	I�B	K�B	L�B	O�B	T�B	VB	W
B	W
B	XB	ZB	\)B	`BB	e`B	e`B	ffB	ffB	ffB	ffB	ffB	m�B	p�B	q�B	r�B	r�B	r�B	t�B	u�B	w�B	y�B	~�B	�B	�B	�B	�%B	�JB	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�?B	�LB	�XB	�^B	�dB	�^B	�^B	�XB	�LB	�RB	�RB	�XB	�dB	�jB	�wB	�}B	��B	B	ŢB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�/B	�HB	�NB	�TB	�TB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
	7B
	7B

=B
DB
DB
DB
DB
PB
�B
�B
%B
,�B
7�B
>�B
E9B
I�B
M�B
U�B
]dB
`�B
e�B
k�B
poB
r-B
u�B
yXB
|�B
�B
�G111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
PTB
PVB
PVB
PVB
PXB
PVB
OHB
OQB
OIB
OJB
PWB
Q\B
Q\B
Q]B
Q\B
Q_B
SfB
Y�B
Z�B
Z�B
[�B
\�B
^�B
`�B
a�B
a�B
a�B
`�B
_�B
^�B
^�B
_�B
c�B
d�B
e�B
j�B
`�B
6�B
B	�dB
�B
<�B
j�B
��B
�"B
�iB
ۙB
��B
ߴB
׀B
�zB
�B
�#B
�LB tB
�ZB|B	�B�BB)kB7�BK4BY�Be�Bs#B�qB��B��B��B�QB�sB��B�:B�BB:�BDBI'BW�BZ�Be�B{NB��B��B��B��B��B~fBt+BnB^�BSdBd�B�B��B��BY�B6�B
�B�B�8B�8B��B�@Bg�BpB�tBj�BTnBDB
B
�:B
�fB
�HB
��B
�hB
�B
��B
��B
nB
a�B
Z�B
RjB
NOB
H/B
=�B
+�B
B
�B
 B	ܨB	��B	��B	��B	wKB	i�B	PaB	8�B	)yB	&iB	!DB	)B	B	�B	�B	�B�B�yB�gB�HB�#B�B��B��B��B��B��B��B��BݳBۧB޽B޼BܰBܭBܰB��B��BܮBؘBڤBؖB�zB�fB�aB�GB��B��B��B��B��B�aB�'B�B�B�B��B��B��B��B��B�B��B�B��B��B}vBr0Bp&Bo"Bo$BlBjBjBlBuGBvMBvLB{iB��B��B��B��B��B{kByaBwVBs>B��B�[B�ZB�[B�\B�GB�.B��B��B��B��B��B��B��B~~B{jBxXBzdB��B��B��B��B��B��B{kBy^By\B��B��B��B�B�[B�[B�OB�MB�DB�5B�*B�&B�B�B��B�B�*B�kB�xB�gB�nB�mB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B� B�EB�cB�qB�{BהBکBݺB��B��B��B�B�$B�/B�/B�.B�<B�@B�NB�YB�\B�yB��B��B	�B	�B	
�B	�B	�B	�B	 B	B	B	#B	'B	'B	6B	 KB	"UB	'qB	-�B	.�B	/�B	4�B	5�B	6�B	9�B	:�B	;�B	?B	CB	H8B	JEB	KKB	N[B	S{B	T�B	U�B	U�B	V�B	X�B	Z�B	^�B	c�B	c�B	d�B	d�B	d�B	d�B	d�B	lB	o!B	p'B	q0B	q+B	q/B	s9B	t?B	vOB	xVB	}yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�1B	�6B	�DB	�RB	�ZB	�ZB	�]B	�aB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�"B	�.B	�8B	�5B	�@B	�<B	�JB	�QB	�[B	�bB	�nB	�sB	ԀB	֎B	ۮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�B	�B	�B	�&B	�&B	�$B	�&B	�0B	�AB	�BB	�FB	�OB	�SB	�SB	�WB	�`B	�YB	�YB	�^B	�`B	�bB	�aB	�nB	�vB	�|B	�}B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�G�O�B
]B
)B
#�B
+B
6B
=uB
C�B
HB
L B
T5B
[�B
_BB
d-B
jNB
n�B
p�B
t?B
w�B
{LB
~�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.21 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0.0001), vertically averaged dS =-0.002(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940222019060409402220190604094022  AO  ARCAADJP                                                                    20181121041151    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041151  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041151  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094022  IP                  G�O�G�O�G�O�                