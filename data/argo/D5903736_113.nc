CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-23T19:16:19Z AOML 3.0 creation; 2016-05-31T19:14:43Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150523191619  20160531121443  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               qA   AO  4051_7090_113                   2C  D   APEX                            5368                            041511                          846 @�R�%+ 1   @�R��)��@4�C��%�dI�"��`1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    qA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D��D�9�D���D�ٚD�fD�C3D�� D��fD��D�L�D�y�Dǰ D��D�C3D�VfD��3D�	�D�9�D�s3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @}p�@��@��A�\A:�\AZ�\Az�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��HC��HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D j=D �=Dj=D�=Dj=D�=Dj=D�=Dj=D�Dj=D�=Dj=D�=Dj=D�=Dj=D�=D	j=D	�=D
j=D
�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=D j=D �=D!j=D!�=D"j=D"�=D#j=D#�=D$j=D$�=D%j=D%�=D&j=D&�=D'j=D'�=D(j=D(�=D)j=D)�=D*j=D*�=D+j=D+�=D,j=D,�=D-j=D-�=D.j=D.�=D/j=D/�=D0j=D0�=D1j=D1�=D2j=D2�=D3j=D3�=D4j=D4�=D5j=D5�=D6j=D6�=D7j=D7�=D8j=D8�=D9j=D9�=D:j=D:�=D;j=D;�=D<j=D<�=D=j=D=�=D>j=D>�=D?j=D?�=D@j=D@�=DAj=DA�=DBj=DB�=DCj=DC�=DDj=DD�=DEj=DE�=DFj=DF�=DGj=DG�=DHj=DH�=DIj=DI�=DJj=DJ�=DKj=DK�=DLj=DL�=DMj=DM�=DNj=DN�=DOj=DO�=DPj=DP�=DQj=DQ�=DRj=DR�=DSj=DS�=DTj=DT�=DUj=DU�=DVj=DV�=DWj=DW�=DXj=DX�=DYj=DY�=DZj=DZ�=D[j=D[�=D\j=D\�=D]j=D]�=D^j=D^�=D_j=D_�=D`j=D`�=Daj=Da�=Dbj=Db�=Dcj=Dc�=Ddj=Dd�=Dej=De�=Dfj=Df�=Dgj=Dg�=Dhc�Dh�=Dij=Di�=Djj=Dj�=Dkj=Dk�=Dlj=Dl�=Dmj=Dm�=Dnj=Dn�=Doj=Do�=Dpj=Dp�=Dqj=Dq�=Drj=Dr�=Dsj=Ds�=Dtj=Dt��Dy�=D��D�.�D�~�D�ιD��D�8RD�uD���D��D�A�D�n�DǥD��D�8RD�K�D��RD���D�.�D�hRD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A�v�Aϕ�A���A��A��TA���AΙ�A�hsA�ZA�K�A�&�A��A���A��#Aͺ^A͡�A�p�A�9XA���A�p�A�A�A�?}A�;dA�9XA�=qA�E�A˲-A��A�9XA���A��AɮA�-AȍPA��A�7LA�?}A�\)A��A�O�A�I�A���A��/A¡�A��PA��+A��-A���A�%A�v�A��#A�C�A��A�1A��PA�I�A�A���A��^A�$�A�-A��wA�^5A��/A�^5A��A��TA��\A���A�n�A��PA��A���A��A�XA���A�
=A�bNA�C�A���A�"�A��A���A�jA��yA�XA�A��hA�A�A��A���A�jA���A��!A�n�A�p�A��hA�I�A�VA�z�A��A�&�A��A�7LA�VA��A�1'A��DA�=qA���A�E�A�5?A�5?A���A��A���A�t�A��/A�XA��A��^A�{A��A���A��#A���A�=qA�C�A�ĜA��\A�"�A�5?AA}��A{��AxI�Aq?}Ao�7Ao%Am�PAl$�Ae7LAc�
Abr�A_��A\��AZ�+AY��AYx�AY
=AW"�AU�AS/AQ\)AO�TAMXAI��AH��AH�AFȴAA��A?A=\)A=%A<r�A;�;A;�A;oA9�#A7�A6�RA5G�A3�^A1�PA0A//A-�mA,��A+��A+��A+G�A+oA+%A*�A)t�A(�A(1A'��A'��A'|�A'S�A'/A&�A%�A"��A ��A\)AA�AXAȴA=qA�yAI�A33Av�A��AbNA�\A7LA�HA
�A
ZA	�#A�+A5?A�wA��A^5Az�A ff@�ȴ@��^@�;d@��@��@�dZ@��@���@��@�9X@�n�@��/@�o@�-@�Z@���@�\)@�{@�V@�j@���@�@��@��@�7L@��u@�ƨ@ݑh@�5?@���@�|�@�5?@���@��T@���@�/@��/@��@Ӆ@�t�@��y@Ѳ-@��/@��@͡�@�G�@��/@�I�@�33@�~�@�{@���@�7L@ȼj@�b@ǶF@�
=@�v�@��@ļj@�S�@�5?@�/@��w@�o@�@��H@���@��R@�-@�p�@�Ĝ@��;@�S�@��@�n�@�hs@�j@���@�S�@���@��R@���@�=q@�@��@�V@���@��9@��D@���@�1@��@���@�=q@���@���@��7@�G�@�Ĝ@�z�@�(�@��@�|�@�S�@�@��H@�~�@�M�@��@���@��^@�O�@�?}@�7L@�&�@���@��@�(�@��m@�\)@���@��y@��y@��y@��y@��@���@���@��\@�M�@��@�V@��@���@���@��u@��D@�j@�Z@�(�@�ƨ@��@�+@��H@�V@�E�@�-@��#@�`B@��@�%@��@��m@��@�S�@�C�@�+@��@���@���@��\@�M�@��T@�p�@�`B@�7L@�&�@�%@�I�@�9X@�9X@�  @�;d@���@��\@��+@�V@�@��h@��@��@��u@�r�@��@�  @��@�1@��@�1@��m@��
@�ƨ@���@��@�l�@�dZ@��@��@�|�@�|�@�|�@�K�@�+@�
=@���@���@��\@�~�@�M�@�{@�@��h@�hs@�G�@���@���@�r�@�b@��F@�t�@�\)@�;d@�
=@���@��H@���@���@��T@�X@���@��j@���@��@�Q�@�A�@�A�@�A�@�1'@�1'@�  @�ƨ@��P@��@���@�E�@��@��-@�X@�V@�Ĝ@��@��D@�j@�Q�@�1'@�1@�|�@���@�ff@���@�?}@��u@�  @���@�S�@�+@��7@�x�@~{@q�7@h�`@bM�@Z^5@Q7L@G�P@@b@9��@3ƨ@,z�@'\)@"n�@�T@dZ@E�@7L@O�@	�#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A�v�Aϕ�A���A��A��TA���AΙ�A�hsA�ZA�K�A�&�A��A���A��#Aͺ^A͡�A�p�A�9XA���A�p�A�A�A�?}A�;dA�9XA�=qA�E�A˲-A��A�9XA���A��AɮA�-AȍPA��A�7LA�?}A�\)A��A�O�A�I�A���A��/A¡�A��PA��+A��-A���A�%A�v�A��#A�C�A��A�1A��PA�I�A�A���A��^A�$�A�-A��wA�^5A��/A�^5A��A��TA��\A���A�n�A��PA��A���A��A�XA���A�
=A�bNA�C�A���A�"�A��A���A�jA��yA�XA�A��hA�A�A��A���A�jA���A��!A�n�A�p�A��hA�I�A�VA�z�A��A�&�A��A�7LA�VA��A�1'A��DA�=qA���A�E�A�5?A�5?A���A��A���A�t�A��/A�XA��A��^A�{A��A���A��#A���A�=qA�C�A�ĜA��\A�"�A�5?AA}��A{��AxI�Aq?}Ao�7Ao%Am�PAl$�Ae7LAc�
Abr�A_��A\��AZ�+AY��AYx�AY
=AW"�AU�AS/AQ\)AO�TAMXAI��AH��AH�AFȴAA��A?A=\)A=%A<r�A;�;A;�A;oA9�#A7�A6�RA5G�A3�^A1�PA0A//A-�mA,��A+��A+��A+G�A+oA+%A*�A)t�A(�A(1A'��A'��A'|�A'S�A'/A&�A%�A"��A ��A\)AA�AXAȴA=qA�yAI�A33Av�A��AbNA�\A7LA�HA
�A
ZA	�#A�+A5?A�wA��A^5Az�A ff@�ȴ@��^@�;d@��@��@�dZ@��@���@��@�9X@�n�@��/@�o@�-@�Z@���@�\)@�{@�V@�j@���@�@��@��@�7L@��u@�ƨ@ݑh@�5?@���@�|�@�5?@���@��T@���@�/@��/@��@Ӆ@�t�@��y@Ѳ-@��/@��@͡�@�G�@��/@�I�@�33@�~�@�{@���@�7L@ȼj@�b@ǶF@�
=@�v�@��@ļj@�S�@�5?@�/@��w@�o@�@��H@���@��R@�-@�p�@�Ĝ@��;@�S�@��@�n�@�hs@�j@���@�S�@���@��R@���@�=q@�@��@�V@���@��9@��D@���@�1@��@���@�=q@���@���@��7@�G�@�Ĝ@�z�@�(�@��@�|�@�S�@�@��H@�~�@�M�@��@���@��^@�O�@�?}@�7L@�&�@���@��@�(�@��m@�\)@���@��y@��y@��y@��y@��@���@���@��\@�M�@��@�V@��@���@���@��u@��D@�j@�Z@�(�@�ƨ@��@�+@��H@�V@�E�@�-@��#@�`B@��@�%@��@��m@��@�S�@�C�@�+@��@���@���@��\@�M�@��T@�p�@�`B@�7L@�&�@�%@�I�@�9X@�9X@�  @�;d@���@��\@��+@�V@�@��h@��@��@��u@�r�@��@�  @��@�1@��@�1@��m@��
@�ƨ@���@��@�l�@�dZ@��@��@�|�@�|�@�|�@�K�@�+@�
=@���@���@��\@�~�@�M�@�{@�@��h@�hs@�G�@���@���@�r�@�b@��F@�t�@�\)@�;d@�
=@���@��H@���@���@��T@�X@���@��j@���@��@�Q�@�A�@�A�@�A�@�1'@�1'@�  @�ƨ@��P@��@���@�E�@��@��-@�X@�V@�Ĝ@��@��D@�j@�Q�@�1'@�1@�|�@���@�ff@���@�?}@��u@�  @���@�S�@�+@��7@�x�@~{@q�7@h�`@bM�@Z^5@Q7L@G�P@@b@9��@3ƨ@,z�@'\)@"n�@�T@dZ@E�@7L@O�@	�#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB,B+B+B+B-B-B/B33B:^BC�BF�BI�BR�B^5BffBw�B�B�VB��B��B�!B�-B�9B�9B�9B�?B��B
=BQ�Bm�B�%B��B�B�LB�dB��B�ZB�B�B�yB��B�B,B+B(�B%�B-BO�B[#BbNBr�B|�B�%B�=B�+B�%B�PB�DB�PB�+Bp�BM�B?}BD�BI�B_;B`BB_;B^5B]/B]/B[#B\)B_;BbNBl�Bl�Bo�Bq�Bk�BcTB`BB\)BT�BR�Bl�Br�BjBffBiyBffBe`BaHB_;B\)BVBO�B?}B/B+B0!B1'B%�B�BbBB�ZBɺB��B��B��B��B��B��B�-B�%BL�BB�)B�B��Bw�BS�BD�B �B+B
�B
�`B
��B
ÖB
�XB
��B
�\B
r�B
aHB
P�B
8RB
�B	�TB	��B	��B	��B	�dB	�DB	~�B	r�B	ZB	D�B	6FB	1'B	0!B	0!B	+B	%�B	�B	�B	\B	+B��B�B�B�BB��BɺBɺBȴBǮBĜBB��B�qB�dB�^B�^B�RB�qB��BBĜBƨBǮBǮBȴBȴBǮBǮBȴBȴBɺBɺBɺBɺBɺBȴBƨBȴB��B��B��B��B��B��B��B��B��B��B��BȴB�jB�!B��B��B�oB�\B�PB�DB�\B�JB�+B�B~�B|�B|�B{�B{�B|�B}�B|�B|�B{�B|�B}�B}�B}�B� B� B�B�B�B�B�B�B�B�B�B�+B�+B�+B�1B�=B�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�B�B�'B�?B�RB�jB��BŢBǮB��B��B��B��B��BɺBɺB��B��B��B�B�B�B�B�5B�TB�B�B��B��B	B	+B	JB	bB	{B	�B	�B	�B	$�B	(�B	+B	.B	0!B	33B	6FB	6FB	6FB	9XB	<jB	?}B	B�B	B�B	D�B	E�B	F�B	G�B	I�B	K�B	O�B	P�B	Q�B	S�B	T�B	W
B	W
B	ZB	[#B	[#B	^5B	^5B	^5B	^5B	_;B	aHB	cTB	e`B	gmB	jB	k�B	k�B	l�B	l�B	l�B	m�B	m�B	n�B	o�B	s�B	u�B	v�B	v�B	w�B	w�B	w�B	x�B	x�B	y�B	|�B	}�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�=B	�JB	�\B	�bB	�bB	�bB	�hB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�-B	�9B	�?B	�FB	�XB	�^B	�jB	�wB	��B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�)B	�5B	�BB	�HB	�HB	�NB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
+B
hB
�B
�B
&�B
,B
1'B
7LB
?}B
E�B
I�B
P�B
XB
]/B
cTB
gmB
jB
o�B
s�B
w�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B,&B+B+B+ B-,B-+B/8B3OB:yBC�BF�BI�BSB^TBf�Bw�B�:B�pB��B�B�;B�JB�QB�UB�VB�ZB��B
XBRBm�B�AB��B�,B�jB��B�B�zB�B�B�B�B�B,)B+#B)B&B-/BPB[EBbmBr�B}B�IB�`B�OB�HB�xB�hB�tB�OBp�BM�B?�BD�BI�B_\B`dB__B^XB]PB]SB[BB\JB__BbrBl�Bl�Bo�Bq�Bk�BczB`gB\LBU!BSBl�Br�Bj�Bf�Bi�Bf�Be�BaiB__B\JBV)BPB?�B/;B+$B0AB1JB&B�B�B&B�zB��B�B��B��B��B��B��B�OB�FBL�B9B�JB�7B��Bw�BTBD�B �BOB
��B
�B
�B
ùB
�zB
�B
��B
r�B
aqB
QB
8|B
�B	�B	�B	�B	��B	��B	�tB	+B	r�B	ZOB	D�B	6wB	1YB	0TB	0VB	+6B	&B	�B	�B	�B	`B��B��B�B�{B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�"B�$B�B�B�B�&B�&B�B��B��B�]B�(B��B��B��B��B��B��B��B�kB�_B8B},B}-B|'B|&B}.B~2B}-B}.B|&B},B~3B~2B~1B�>B�AB�GB�EB�GB�MB�XB�WB�]B�]B�_B�kB�jB�lB�qB�}B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�#B�>B�aB�YB�ZB�cB�~B��B��B��B��B��B�B�B�B�B�B��B��B��B�B�(B�BB�AB�BB�PB�pB�B��B��B��B�/B	TB	eB	�B	�B	�B	�B	�B	�B	%B	)0B	+=B	.OB	0XB	3kB	6B	6�B	6}B	9�B	<�B	?�B	B�B	B�B	D�B	E�B	F�B	G�B	I�B	K�B	PB	QB	R#B	T1B	U7B	WBB	WAB	ZVB	[\B	[\B	^mB	^nB	^mB	^kB	_rB	a�B	c�B	e�B	g�B	j�B	k�B	k�B	l�B	l�B	l�B	m�B	m�B	n�B	o�B	s�B	u�B	w B	w B	xB	xB	xB	yB	yB	zB	}%B	~+B	�=B	�BB	�IB	�PB	�QB	�UB	�[B	�bB	�bB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�:B	�JB	�PB	�UB	�WB	�[B	�eB	�qB	�rB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�&B	�-B	�3B	�=B	�DB	�IB	�PB	�YB	�^B	�jB	�uB	�{B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	� B	�"B	�#B	� B	�$B	�"B	�!B	�'B	�0B
 1B
 1B
8B
9B
?B
>B
?B
FB
^B
�B
�B
�B
'B
,;B
1ZB
7}B
?�B
E�B
I�B
QB
XDB
]^B
c�B
g�B
j�B
o�B
s�B
w�B
|111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.34 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214432016053112144320160531121443  AO  ARCAADJP                                                                    20150523191619    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150523191619  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150523191619  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121443  IP                  G�O�G�O�G�O�                