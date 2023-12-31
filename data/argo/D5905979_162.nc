CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-07-14T14:00:36Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200714140036  20220204114427  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�(�����1   @�(�����@6t�j~���b�p��
=1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  Dy�D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�'\D�aHD��{D���D�\D�QHD��HD���D�#�D�UqD���D�ɚD�&�D�Z�D�i�D��=D�RD�U�D�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @w
=@��@��A�\A:�\AZ�\Az�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�ǮC��{C��{C��{C��{C��{C��{C�ǮC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D j=D �=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=D	j=D	�=D
j=D
�=Dj=D�=Dj=D�=Dj=D�=Dj=D�Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dc�D�=Dj=D�=Dj=D�Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D�=Dj=D��Dj=D�=D j=D �=D!j=D!�=D"j=D"�=D#j=D#�=D$j=D$�=D%j=D%�=D&j=D&�=D'j=D'�=D(j=D(�=D)j=D)�=D*j=D*�=D+j=D+�=D,j=D,�=D-j=D-�=D.j=D.�=D/j=D/�=D0p�D0�=D1j=D1�=D2j=D2�=D3j=D3�=D4j=D4�=D5j=D5�=D6j=D6�=D7j=D7�=D8j=D8�=D9j=D9�=D:j=D:�=D;j=D;�=D<j=D<�=D=c�D=�=D>j=D>�=D?j=D?�=D@j=D@�=DAj=DA�=DBj=DB�=DCj=DC�=DDj=DD�=DEj=DE�=DFj=DF�=DGj=DG�=DHj=DH�=DIj=DI�=DJj=DJ�=DKj=DK�=DLj=DL�DMj=DM�=DNj=DN�=DOj=DO�=DPj=DP�=DQj=DQ�=DRj=DR�=DSj=DS�=DTj=DT�=DUj=DU�=DVj=DV�=DWj=DW�=DXj=DX�=DYj=DY�=DZj=DZ�=D[j=D[�=D\j=D\�=D]j=D]�=D^j=D^�=D_j=D_�=D`j=D`�=Daj=Da�=Dbj=Db�=Dcj=Dc�=Ddj=Dd�=Dej=De�=Dfj=Df�=Dgj=Dg�=Dhj=Dh�=Dij=Di�=Djj=Dj�=Dkj=Dk�=Dlj=Dl�=Dmj=Dm�=Dnj=Dn�=Doj=Do�=Dpj=Dp�=Dqj=Dq�=Drj=Dr�=Dsj=Ds�=Dtj=Dt�
Dy��D�{D�VgD�y�D���D�{D�FgD��gD�� D��D�J�D��DǾ�D��D�P D�_D��\D�qD�J�D��D�Å111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�JA�JA��A� �A� �A��A��A��A�-A�+A�+A�&�A�$�A�&�A�(�A�(�A�&�A�(�A�+A�(�A�&�A�"�A�JAʡ�A�I�AƉ7A��#A�=qA�VA���A�+A�ĜA��A��A�`BA��
A�Q�A���A���A���A���A�{A���A���A�$�A��uA���A�5?A�A��!A�z�A� �A��#A���A���A���A���A��A�A���A��DA���A�r�A���A�VA��A�x�A��#A��!A�E�A�hsA�ffA��^A�VA�l�A�=qA��A�ZA��^A���A�1A��A�$�A�A��A�%A�x�A�VA�E�A�hsA�A�ZA��A�&�A�x�A��PA��/A��7A�
=A��HA���A��A��A�
=A�S�A�JA�bA��DA�
=A��^A��A~��A|��AzVAw�hAudZAt��As;dArJAnz�Ak&�Ait�Ae��Ac33AbbAaXA_l�A^��A]dZAY��AV��ATffAR5?AN��AL��AK?}AJAH��AG+AE�ADbNACABjA@M�A>�A=S�A;A:bA8��A6  A3�A21'A0JA.�`A-��A,ĜA+K�A)��A(��A(bNA'`BA%�A%33A$n�A#�^A"�jA"5?A"  A!|�A �A��A�/A(�A�FA�A\)A33A��Ap�A�A�RAZA�wA�yA�wAp�A/A?}AbNA  A9XA��A�A$�A�^A�AO�Av�A�A�uA	��AVA�A�/A��A�\A�A��A7LAv�A =q@��y@��\@�O�@��@�ff@���@���@�n�@��@���@�@�^@��@�+@���@�@��@�ƨ@��
@�S�@�(�@�t�@�/@�b@��@畁@��T@�bN@�@��@�%@��@�o@�n�@�E�@�=q@�9X@�
=@�V@ف@ج@��m@�ȴ@ղ-@�p�@���@� �@�\)@���@�E�@Ѻ^@�;d@�V@�7L@�j@���@�X@�ƨ@Ƈ+@��#@��@�t�@�=q@���@���@�Z@���@���@�`B@��@��@��^@�Ĝ@�r�@�ƨ@�C�@�S�@�33@�"�@�@�5?@��@�bN@�  @���@�o@���@��h@�V@���@�
=@��@���@��@�X@��u@�Q�@�I�@�b@��P@��@��+@��T@�hs@�V@��j@�A�@��P@�S�@�+@�o@���@�$�@��^@�&�@��u@�1'@���@�S�@��@���@��@�@��h@�O�@��@���@�A�@�b@��m@��P@�;d@�o@��y@��!@��\@�E�@��T@��h@�V@�bN@�Z@�bN@�1'@�1@��F@��@�K�@���@���@��R@�~�@�M�@�-@��@�@���@�x�@�p�@�hs@�hs@�G�@�&�@�V@���@��9@�z�@�Z@�Q�@�1'@���@��@�|�@�\)@�C�@�"�@�
=@��y@���@�~�@�^5@�=q@�$�@�J@��@���@���@�p�@�X@�V@��`@���@�Q�@�  @���@�K�@�;d@�"�@���@�-@�$�@�@��-@�?}@���@�A�@�1@��;@��w@��@�|�@�"�@�;d@�;d@�33@�"�@���@�^5@�5?@��#@�G�@��@���@��@��@�r�@�9X@�1@��@��F@��@�l�@�dZ@�C�@�@���@��R@��R@���@�~�@�E�@�$�@�J@��@��-@�x�@�O�@�/@�7L@��/@���@��D@�j@�I�@�  @��F@�|�@�;d@�33@��y@���@�=q@�$�@��-@��h@��7@��7@��7@�`B@��j@��@��9@��9@��9@�z�@�A�@�(�@��@�'R@z8�@t:�@m��@d,=@[{J@Q��@Kj�@D�`@?H�@7��@1�@,��@%��@"	@k�@=q@��@J�@>�@,=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�JA�JA��A� �A� �A��A��A��A�-A�+A�+A�&�A�$�A�&�A�(�A�(�A�&�A�(�A�+A�(�A�&�A�"�A�JAʡ�A�I�AƉ7A��#A�=qA�VA���A�+A�ĜA��A��A�`BA��
A�Q�A���A���A���A���A�{A���A���A�$�A��uA���A�5?A�A��!A�z�A� �A��#A���A���A���A���A��A�A���A��DA���A�r�A���A�VA��A�x�A��#A��!A�E�A�hsA�ffA��^A�VA�l�A�=qA��A�ZA��^A���A�1A��A�$�A�A��A�%A�x�A�VA�E�A�hsA�A�ZA��A�&�A�x�A��PA��/A��7A�
=A��HA���A��A��A�
=A�S�A�JA�bA��DA�
=A��^A��A~��A|��AzVAw�hAudZAt��As;dArJAnz�Ak&�Ait�Ae��Ac33AbbAaXA_l�A^��A]dZAY��AV��ATffAR5?AN��AL��AK?}AJAH��AG+AE�ADbNACABjA@M�A>�A=S�A;A:bA8��A6  A3�A21'A0JA.�`A-��A,ĜA+K�A)��A(��A(bNA'`BA%�A%33A$n�A#�^A"�jA"5?A"  A!|�A �A��A�/A(�A�FA�A\)A33A��Ap�A�A�RAZA�wA�yA�wAp�A/A?}AbNA  A9XA��A�A$�A�^A�AO�Av�A�A�uA	��AVA�A�/A��A�\A�A��A7LAv�A =q@��y@��\@�O�@��@�ff@���@���@�n�@��@���@�@�^@��@�+@���@�@��@�ƨ@��
@�S�@�(�@�t�@�/@�b@��@畁@��T@�bN@�@��@�%@��@�o@�n�@�E�@�=q@�9X@�
=@�V@ف@ج@��m@�ȴ@ղ-@�p�@���@� �@�\)@���@�E�@Ѻ^@�;d@�V@�7L@�j@���@�X@�ƨ@Ƈ+@��#@��@�t�@�=q@���@���@�Z@���@���@�`B@��@��@��^@�Ĝ@�r�@�ƨ@�C�@�S�@�33@�"�@�@�5?@��@�bN@�  @���@�o@���@��h@�V@���@�
=@��@���@��@�X@��u@�Q�@�I�@�b@��P@��@��+@��T@�hs@�V@��j@�A�@��P@�S�@�+@�o@���@�$�@��^@�&�@��u@�1'@���@�S�@��@���@��@�@��h@�O�@��@���@�A�@�b@��m@��P@�;d@�o@��y@��!@��\@�E�@��T@��h@�V@�bN@�Z@�bN@�1'@�1@��F@��@�K�@���@���@��R@�~�@�M�@�-@��@�@���@�x�@�p�@�hs@�hs@�G�@�&�@�V@���@��9@�z�@�Z@�Q�@�1'@���@��@�|�@�\)@�C�@�"�@�
=@��y@���@�~�@�^5@�=q@�$�@�J@��@���@���@�p�@�X@�V@��`@���@�Q�@�  @���@�K�@�;d@�"�@���@�-@�$�@�@��-@�?}@���@�A�@�1@��;@��w@��@�|�@�"�@�;d@�;d@�33@�"�@���@�^5@�5?@��#@�G�@��@���@��@��@�r�@�9X@�1@��@��F@��@�l�@�dZ@�C�@�@���@��R@��R@���@�~�@�E�@�$�@�J@��@��-@�x�@�O�@�/@�7L@��/@���@��D@�j@�I�@�  @��F@�|�@�;d@�33@��y@���@�=q@�$�@��-@��h@��7@��7@��7@�`B@��j@��@��9@��9@��9@�z�@�A�@�(�G�O�@�'R@z8�@t:�@m��@d,=@[{J@Q��@Kj�@D�`@?H�@7��@1�@,��@%��@"	@k�@=q@��@J�@>�@,=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
� B
�B
�B
�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�=B
�B
��B�B0!B:^BH�B^5BdZBgmB��BĜB��B�B�B��BŢB�RB�}BÖBĜB��B��B��BȴB��B��B��B��B��B��B�B�;B�B��BB	7B�B!�B$�B-B49B<jBB�BC�BD�BI�BL�BS�B_;B^5BbNBcTBVBE�B9XB%�BoB��B�mB�dB�JB�BiyB_;BO�BF�B=qB7LB+B+B$�B �B�B%B
�B
��B
�wB
�!B
�3B
��B
��B
��B
�7B
m�B
dZB
J�B
>wB
33B
�B
B
  B	�B	�B	�TB	�B	ȴB	�B	��B	�1B	t�B	m�B	l�B	`BB	YB	O�B	<jB	&�B	�B	+B��B�B�ZB�5B�B��B��BŢBB��B�dB�9B�B��B��B��B�\B� Bx�Bn�BhsBiyBl�B{�B�B|�Bz�Bx�Bp�Bm�BgmBdZBaHB_;B]/B[#BXBVBP�BO�BM�BL�BK�BJ�BJ�BH�BO�BI�BF�BC�B@�BB�BJ�BK�BP�BR�B\)BgmBgmBl�Bl�Bl�Bw�B� B}�Bo�BhsB^5BQ�BN�BI�BB�B?}B<jB:^B;dB;dB5?B2-B49B6FB33B33B2-B2-B5?B7LB6FB5?B7LB6FB8RB:^B?}BA�BB�BD�BK�BT�BZBXBW
B\)BgmBjBk�BhsBffBffBiyBjBm�Bn�Bo�Br�Bq�Bq�Bq�Bq�Bq�Bq�Bp�Bo�Bo�Bo�Bp�Bp�Bo�Bo�Bv�Bw�Bz�B|�B|�B}�B}�B{�B~�B|�B}�B}�B}�B}�B}�B~�B~�B~�B~�B�B�B�%B�+B�1B�7B�\B�{B��B��B��B��B��B��B�B�B�-B�-B�3B�9B�LB�^B�^B�dB�dB�}B��B��B��BÖBĜBƨBȴB��B��B��B��B��B�B�
B�B�#B�5B�HB�ZB�sB�B�B�B�B��B��B��B��B	B	B	1B	JB	VB	bB	{B	�B	�B	�B	 �B	!�B	%�B	+B	/B	33B	:^B	;dB	;dB	=qB	?}B	C�B	F�B	I�B	M�B	O�B	Q�B	T�B	XB	YB	[#B	\)B	\)B	]/B	]/B	]/B	^5B	^5B	`BB	aHB	bNB	dZB	hsB	iyB	jB	k�B	o�B	q�B	t�B	u�B	v�B	x�B	x�B	y�B	|�B	~�B	�B	�B	�B	�B	�%B	�+B	�7B	�DB	�JB	�PB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�FB	�^B	�wB	�wB	�wB	�}B	B	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�5B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�NB	�TB	�TB	�ZB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B
�B
�B
dB
%�B
.}B
7�B
>]B
GB
NVB
U�B
[	B
`\B
gB
iDB
m�B
rB
v`B
z^B
|�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
p�B
p�B
ozB
p�B
p�B
p�B
ozB
ozB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
y�B
��B
�`BB�B)�B8BM�BS�BV�B�B��B�KB�bB�jB�@B��B��B��B��B��B�B�5B�5B�B�B�*B�6B�CB�IB�OB�mBΑB��B�4B�kB��B�BB,B\B#�B+�B1�B2�B3�B9B<BCCBN�BMBQ�BR�BEPB4�B(�B7B�B�NB��B��B{�Br~BX�BN�B?SB6B,�B&�B|B|BXBAB	B
��B
�"B
�tB
�B
��B
��B
�xB
�1B
�B
x�B
])B
S�B
:^B
.B
"�B
JB	��B	�B	�aB	�=B	�B	��B	�eB	��B	��B	w�B	dzB	]QB	\KB	PB	H�B	?�B	,2B	�B	ZB��B�B�`B�0B�B��B��B��B�}B�kB�`B�BB�B��B��B��B��BBBo�Bh�B^�BX_BYeB\wBk�BtBl�Bj�Bh�B`�B]BW\BTJBQ8BO,BM BKBHBE�B@�B?�B=�B<�B;�B:�B:�B8�B?�B9�B6�B3�B0zB2�B:�B;�B@�BB�BLBW`BW`B\~B\~B\~Bg�Bo�Bm�B_�BXhBN,BA�B>�B9�B2�B/yB,gB*[B+aB+bB%>B"-B$8B&EB#3B#3B"-B"-B%?B'LB&FB%@B'MB&GB(SB*_B/}B1�B2�B4�B;�BD�BJBHBGBL'BWiBZ{B[�BXpBVcBVcBYvBZ|B]�B^�B_�Bb�Ba�Ba�Ba�Ba�Ba�Ba�B`�B_�B_�B_�B`�B`�B_�B_�Bf�Bg�Bj�Bl�Bl�Bm�Bm�Bk�Bn�Bl�Bm�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�BqBuBv$Bw*Bx0By6BZB�yB�B��B��B��B��B��B��B�B�(B�(B�.B�4B�GB�XB�XB�^B�^B�wB�}B�}B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�,B�?B�QB�iB�{B��B�B�B�B��B��B��B��B�B�$B�<B�HB	 TB	lB	~B		�B	�B	�B	�B	�B	�B		B	#!B	*KB	+QB	+QB	-^B	/jB	3�B	6�B	9�B	=�B	?�B	A�B	D�B	G�B	IB	KB	LB	LB	MB	MB	MB	NB	NB	P+B	Q1B	R7B	TCB	X[B	YaB	ZgB	[mB	_�B	a�B	d�B	e�B	f�B	h�B	h�B	i�B	l�B	n�B	p�B	q�B	s�B	s�B	v
B	wB	yB	{(B	|.B	}4B	@B	�FB	�XB	�dB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�&B	�=B	�VB	�VB	�VB	�\B	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�5B	�AB	�HB	�MB	�MB	�SB	�YB	�YB	�_B	�eB	�kB	�rB	�xB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�G�O�B	��B	��B
xB
:B
�B
RB
'�B
.1B
6�B
>)B
E�B
J�B
P.B
V�B
YB
]�B
a�B
f0B
j.B
l�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.34 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.016(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144272022020411442720220204114427  AO  ARCAADJP                                                                    20200714140036    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200714140036  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200714140036  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114427  IP                  G�O�G�O�G�O�                