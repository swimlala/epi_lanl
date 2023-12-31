CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:13:55Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141355  20220204114417  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               CA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @رܑ(1   @ر�""0v@6WKƧ��dI�^5?1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    CA   B   B   @�  @�  @���A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9  D9� D:  D:�fD;  D;� D<  D<� D=  D=� D>  D>� D?fD?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy� D�3D�i�D��\D�ۅD��D�` D��)D��fD�!�D�UD��
D��=D�!�D�U�DچfD���D��D�\{D�\D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��A�\A:�\AZ�\Az�\A�z�A�G�A�G�A�G�A�G�A�G�A�G�A�zB��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B߅B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{�\C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��HC��HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�ǮC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D j=D �=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=D	j=D	�=D
j=D
�=Dj=D�=Dp�D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=D j=D �=D!j=D!�=D"j=D"�=D#j=D#�=D$j=D$�=D%j=D%�=D&j=D&�=D'j=D'�=D(j=D(�=D)j=D)�=D*j=D*�D+j=D+�=D,j=D,�=D-j=D-�=D.j=D.�=D/j=D/�=D0j=D0�=D1j=D1�=D2j=D2�=D3j=D3�=D4j=D4�=D5j=D5�=D6j=D6�=D7j=D7�=D8p�D8�=D9j=D9�=D:p�D:�=D;j=D;�=D<j=D<�=D=j=D=�=D>j=D>�D?j=D?�=D@j=D@�=DAj=DA�=DBj=DB�=DCj=DC�=DDj=DD�=DEj=DE�=DFj=DF�=DGj=DG�=DHj=DH�=DIj=DI�=DJj=DJ�=DKj=DK�=DLj=DL�=DMp�DM�=DNj=DN�=DOj=DO�=DPj=DP�=DQj=DQ�=DRj=DR�=DSj=DS�=DTj=DT�=DUj=DU�=DVj=DV�=DWj=DW�=DXj=DX�=DYj=DY�=DZj=DZ�=D[j=D[�=D\j=D\�=D]j=D]�=D^j=D^�=D_j=D_�=D`j=D`�=Daj=Da�=Dbj=Db�=Dcj=Dc�=Ddj=Dd�=Dej=De�=Dfj=Df�=Dgj=Dg�=Dhj=Dh�=Dij=Di�=Djj=Dj�=Dkj=Dk�=Dlj=Dl�=Dmj=Dm�=Dnj=Dn�=Doj=Do�=Dpj=Dp�=Dqj=Dq�=Drj=Dr�=Dsj=Ds�=Dtj=Dt��Dy�=D�RD�_D��{D�ФD��D�UD��HD�ÅD��D�J>D��)D��\D��D�J�D�{�D��D��D�Q�D�{D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A��`A��A�%A��A��yA��A���A���A��jA��\A�v�A�VA�;dA�"�A�{A�%A�A��`A��RA�`BA��yA���A���A��RA���A���A��+A�x�A�jA�O�A�33A� �A��A�%A��9A�z�A�JA���A�A�hsA�33A�JA��A��A�x�A�C�A��yA���A��uA��A�7LA�l�A�5?A��A�M�A���A�/A��/A��yA�\)A�bA�"�A�XA�dZA�A��A���A��
A��mA�p�A�ƨA�A��PA��FA���A���A�v�A�bNA��#A�bA�ĜA�7LA�l�A�1'A���A�p�A�  A�z�A���A�9XA�ZA��PA�v�A���A���A���A�&�A���A�dZA���A�M�A�A}�mA{/AyVAw�Ap�HAn��Am��AmS�AmVAiO�Ae��Aa;dA_G�A]G�AYhsAV��AUl�AU�AU�AU%AT��AT�yAT�RATM�AS�AR�9AN��AN1AM+ALbNAJA�AHĜAHbNAG�
AGVAF��AF=qABbNA@��A@E�A>�HA=�
A<�+A:E�A8�A8�A7��A7&�A6ĜA6 �A5G�A4��A25?A/O�A.9XA,�A,�A+�7A*�A'��A&-A%oA$(�A"��A ��A�^AdZA��A�#A�Av�A��A �A��A��AXA�HA5?A��A�Ax�A�HA{A��A��A1A�-A;dA
�`A
�DA
E�A	��A	��A	|�A	`BA��A�A�A�
A�AdZA�yA��AĜA9XA��A ��A A�@���@�x�@��@���@��m@�\)@��R@���@���@���@��9@�S�@��T@�Q�@�l�@�=q@��`@�t�@���@�j@��@���@���@�$�@��@�@��@���@���@ݺ^@ݑh@�p�@�V@�r�@�|�@�;d@ڰ!@ڗ�@ڏ\@�p�@�1@�S�@�ȴ@֧�@և+@���@� �@��;@��;@��
@�dZ@��y@�V@�Ĝ@ΰ!@��@�X@ʇ+@Ǯ@�;d@ŉ7@ģ�@��@�;d@�x�@���@���@�1'@��@���@�Ĝ@��9@�I�@�K�@��R@���@�1'@�\)@�+@���@���@��R@�~�@�-@��#@��@���@��@�Z@� �@��F@���@��#@��7@�?}@�V@��/@�Ĝ@�I�@��w@���@�|�@��@�ȴ@���@�V@�5?@��@���@��7@��@�?}@�V@��9@��@��@��@��@�|�@�|�@�S�@�~�@���@��/@� �@��@��
@��F@�|�@��y@�{@�x�@�%@��9@�z�@�I�@��;@�+@��R@�n�@�^5@�5?@��T@��^@��h@�/@�V@���@��j@�|�@�+@��@�o@���@�5?@���@�@���@���@��7@�X@�%@���@��@�bN@�Q�@�1@�\)@�K�@�C�@�K�@�+@���@�ȴ@��R@���@���@�@�@��@�33@���@���@�ȴ@���@�{@��@���@�G�@�%@��m@��@�1@� �@��@��@��m@�ƨ@���@��@��!@���@�V@�J@�x�@�bN@�  @���@�l�@�33@�ƨ@�(�@�1'@�(�@�  @���@�;d@�~�@�E�@�=q@��@��@���@�`B@�?}@�?}@�&�@���@��/@��D@�I�@� �@���@��@�n�@�=q@��@�@��7@���@���@�|�@�@�E�@�@���@���@���@��h@�hs@�%@��9@�j@�ƨ@��@�ȴ@�~�@�5?@��@��+@��\@�$�@��@�x�@��/@��9@���@��D@��@��@l�@�@~v�@~5?@~v�@~��@~V@}�@~�@w+@oo@f��@]�3@Tb@HFt@@e�@;�@47@.Z�@(�U@$�@!rG@��@)_@�1@��@x@A�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�A�A��`A��A�%A��A��yA��A���A���A��jA��\A�v�A�VA�;dA�"�A�{A�%A�A��`A��RA�`BA��yA���A���A��RA���A���A��+A�x�A�jA�O�A�33A� �A��A�%A��9A�z�A�JA���A�A�hsA�33A�JA��A��A�x�A�C�A��yA���A��uA��A�7LA�l�A�5?A��A�M�A���A�/A��/A��yA�\)A�bA�"�A�XA�dZA�A��A���A��
A��mA�p�A�ƨA�A��PA��FA���A���A�v�A�bNA��#A�bA�ĜA�7LA�l�A�1'A���A�p�A�  A�z�A���A�9XA�ZA��PA�v�A���A���A���A�&�A���A�dZA���A�M�A�A}�mA{/AyVAw�Ap�HAn��Am��AmS�AmVAiO�Ae��Aa;dA_G�A]G�AYhsAV��AUl�AU�AU�AU%AT��AT�yAT�RATM�AS�AR�9AN��AN1AM+ALbNAJA�AHĜAHbNAG�
AGVAF��AF=qABbNA@��A@E�A>�HA=�
A<�+A:E�A8�A8�A7��A7&�A6ĜA6 �A5G�A4��A25?A/O�A.9XA,�A,�A+�7A*�A'��A&-A%oA$(�A"��A ��A�^AdZA��A�#A�Av�A��A �A��A��AXA�HA5?A��A�Ax�A�HA{A��A��A1A�-A;dA
�`A
�DA
E�A	��A	��A	|�A	`BA��A�A�A�
A�AdZA�yA��AĜA9XA��A ��A A�@���@�x�@��@���@��m@�\)@��R@���@���@���@��9@�S�@��T@�Q�@�l�@�=q@��`@�t�@���@�j@��@���@���@�$�@��@�@��@���@���@ݺ^@ݑh@�p�@�V@�r�@�|�@�;d@ڰ!@ڗ�@ڏ\@�p�@�1@�S�@�ȴ@֧�@և+@���@� �@��;@��;@��
@�dZ@��y@�V@�Ĝ@ΰ!@��@�X@ʇ+@Ǯ@�;d@ŉ7@ģ�@��@�;d@�x�@���@���@�1'@��@���@�Ĝ@��9@�I�@�K�@��R@���@�1'@�\)@�+@���@���@��R@�~�@�-@��#@��@���@��@�Z@� �@��F@���@��#@��7@�?}@�V@��/@�Ĝ@�I�@��w@���@�|�@��@�ȴ@���@�V@�5?@��@���@��7@��@�?}@�V@��9@��@��@��@��@�|�@�|�@�S�@�~�@���@��/@� �@��@��
@��F@�|�@��y@�{@�x�@�%@��9@�z�@�I�@��;@�+@��R@�n�@�^5@�5?@��T@��^@��h@�/@�V@���@��j@�|�@�+@��@�o@���@�5?@���@�@���@���@��7@�X@�%@���@��@�bN@�Q�@�1@�\)@�K�@�C�@�K�@�+@���@�ȴ@��R@���@���@�@�@��@�33@���@���@�ȴ@���@�{@��@���@�G�@�%@��m@��@�1@� �@��@��@��m@�ƨ@���@��@��!@���@�V@�J@�x�@�bN@�  @���@�l�@�33@�ƨ@�(�@�1'@�(�@�  @���@�;d@�~�@�E�@�=q@��@��@���@�`B@�?}@�?}@�&�@���@��/@��D@�I�@� �@���@��@�n�@�=q@��@�@��7@���@���@�|�@�@�E�@�@���@���@���@��h@�hs@�%@��9@�j@�ƨ@��@�ȴ@�~�@�5?@��@��+@��\@�$�@��@�x�@��/@��9@���@��D@��@��@l�@�@~v�@~5?@~v�@~��@~VG�O�@~�@w+@oo@f��@]�3@Tb@HFt@@e�@;�@47@.Z�@(�U@$�@!rG@��@)_@�1@��@x@A�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B�?BĜBĜBĜBĜBBBB��BBBŢBȴB��B��B��B��B��B�B�BBB	7BDBJBPBPBPBVB�B�B�B�B�B"�B!�B(�B2-B>wBK�BO�BQ�BS�BXB]/BbNBiyBl�Bl�Br�Bu�Bt�Bt�B^5Bv�Bw�Bv�Br�B`BBL�BD�B@�B;dB&�B�BhBB��B�B�yB�BB�B��BB�dB��B��B��B��B�uB�\B�+B}�By�Bu�BjBffBaHBT�B2-B%�B�B\BB
��B
�B
�}B
�B
�B
��B
��B
~�B
iyB
P�B
8RB
.B	�B	��B	ȴB	��B	�XB	��B	}�B	^5B	K�B	;dB	%�B	{B	
=B	1B	1B	+B	+B	%B	B	B��B��B�sB�;B�B��B��BÖB��B�}B�^B�LB�FB�'B��B��B��B�{B�bB�=B�B� B|�B{�By�Bx�Bv�Bs�Bs�Bl�Bm�Bm�Bm�Bk�BjBhsBdZBaHB`BB`BB`BB^5B[#BVBS�BQ�BQ�BN�BL�BK�BK�BJ�BI�BI�BG�BF�BE�BE�BD�BD�B@�B?}B>wB>wB>wB=qB<jB<jB;dB;dB:^B9XB:^B8RB7LB6FB6FB7LB5?B33B2-B2-B2-B0!B1'B49B49B49B49B49B49B5?B8RB8RB9XB:^B<jBA�BF�BI�BH�BF�BK�BN�BL�BP�BQ�BP�BP�BP�BP�BO�BO�BO�BO�BN�BO�BP�BR�BT�BW
BZB]/BbNBffBffBe`BdZBcTBcTBffBhsBiyBjBl�Bl�Bm�Bq�Bt�Bt�Bu�B|�B� B� B�B�+B�7B�DB�bB��B��B��B��B��B��B��B��B�B�B�'B�LB�^B�^B�dB�jB�jB�qB�}B��BBƨB��B��B��B��B��B��B�B�B�#B�)B�#B�/B�/B�/B�/B�BB�NB�TB�`B�mB�yB�B�B�B�B�B��B��B��B��B��B��B��B��B	B	
=B	bB	�B	�B	�B	�B	�B	!�B	'�B	+B	/B	1'B	2-B	33B	6FB	;dB	>wB	@�B	@�B	B�B	D�B	D�B	F�B	H�B	I�B	I�B	J�B	T�B	W
B	XB	XB	YB	^5B	aHB	aHB	bNB	bNB	cTB	e`B	gmB	jB	jB	k�B	m�B	p�B	r�B	s�B	t�B	u�B	w�B	x�B	y�B	z�B	{�B	~�B	�B	�B	�B	�B	�+B	�DB	�DB	�JB	�\B	�\B	�bB	�uB	��B	�uB	�uB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�FB	�dB	�qB	�qB	�wB	�}B	��B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�)B	�)B	�)B	�)B	�)B	�)B	�/B	�;B	�;B	�BB	�HB	�ZB	�`B	�`B	�`B	�`B	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�?B	�B
�B
�B
!�B
,�B
8RB
@iB
E�B
LB
Q�B
V�B
[	B
^�B
d�B
i�B
ncB
r-B
uZB
zDB
}q111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�tB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�7B�B�7B�PBgBtBzB�B�B�B�B�B�B�B�B�B B�B!%B*[B6�BC�BHBJBL%BP=BU[BZzBa�Bd�Bd�Bj�Bm�Bl�Bl�G�O�Bn�Bo�Bn�Bj�BXpBD�B<�B8�B3�BB�B	�B�OB�B��B�B�{B�>B�B��B��B�9B� B�B�B��B��BkBv4BrBnBb�B^�BY�BMBB*tB*B�B�B
�VB
� B
��B
��B
�lB
�YB
�4B
��B
wNB
a�B
I=B
0�B
&nB	�	B	�FB	�B	��B	��B	�)B	v[B	V�B	D2B	3�B	QB	�B	�B	 �B	 �B��B��B��B��B��B�kB�_B��B׮BґB�rB�HB�B��B��B��B��B��B��B�7B�7B�B��B��B��B{�Bx|BujBtcBrXBqRBoFBl3Bl4Be	BfBfBfBdBb�B`�B\�BY�BX�BX�BX�BV�BS�BN�BL{BJoBJpBG]BEQBDKBDKBCEBB?BB?B@3B?-B>'B>'B="B="B9	B8B6�B6�B6�B5�B4�B4�B3�B3�B2�B1�B2�B0�B/�B.�B.�B/�B-�B+�B*�B*�B*�B(�B)�B,�B,�B,�B,�B,�B,�B-�B0�B0�B1�B2�B4�B:B?1BBCBA>B?2BDQBGcBEWBIoBJvBIoBIoBIoBIoBHiBHiBHiBHiBGcBHiBIoBK|BM�BO�BR�BU�BZ�B^�B^�B]�B\�B[�B[�B^�B`�BbBc	BeBeBfBj4BmFBmFBnMBuwBx�Bx�B}�B�B��B��B��B�#B�5B�AB�TB�qB�yB�yB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�/B�`B�fB�fB�fB�yBͅB΋BўBӪBԯBӪBյBնBնBնB��B��B��B��B��B��B�B�*B�*B�5B�;B�HB�`B�sB�yB�yB�yB�yB�B��B	�B	�B	B	B	$B	*B	5B	NB	 sB	#�B	'�B	)�B	*�B	+�B	.�B	3�B	6�B	9B	9B	;B	=B	=B	?*B	A5B	B;B	B;B	CBB	MB	O�B	P�B	P�B	Q�B	V�B	Y�B	Y�B	Z�B	Z�B	[�B	]�B	_�B	b�B	b�B	dB	fB	i$B	k/B	l5B	m;B	nBB	pNB	qTB	rZB	s`B	tfB	wyB	y�B	z�B	y�B	}�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�/B	�HB	�mB	�mB	�sB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�/B	�<B	�<B	�NB	�ZB	�`B	�`B	�`B	�`B	�gB	�gB	�mB	�sB	�yB	�yB	�B	ЊB	υB	ЋB	ЋB	ЋB	ЋB	ӞB	ԣB	ԤB	ժB	ժB	ժB	ժB	ժB	ԤB	ԤB	ԤB	ԤB	ԤB	ԤB	ժB	׶B	׶B	ؽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�G�O�B	��B	��B
#B
TB
CB
%RB
0�B
8�B
>B
D�B
I�B
OLB
SB
V�B
]9B
b=B
f�B
j�B
m�B
r�B
u�111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.34 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144172022020411441720220204114417  AO  ARCAADJP                                                                    20200618141355    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141355  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141355  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114417  IP                  G�O�G�O�G�O�                