CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:25Z creation      
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170925  20220204114424  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�w؜�1   @�q�p@7-�����b��7Kƨ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-�fD.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�'
D�]qD���D��{D��D�c3D���D��D�{D�R=D��3Dǯ\D�\D�R�Dښ=D��D��D�T)D�)D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@��@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB'��B/��B7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-��C/�\C1�\C3�\C5�\C7��C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[��C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(�qD)s�D)��D*s�D*��D+s�D+��D,s�D,��D-z=D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMmqDM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW�=DXz=DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di�=Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dy��D� �D�W]D���D��gD��D�]D���D��D�gD�L)D�}DǩHD�HD�L{Dڔ)D��D�qD�ND�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�(�A�&�A�5?A�?}A�=qA�=qA�A�A�A�A�A�A�C�A�C�A�A�A�C�A�?}A�=qA�=qA�;dA�;dA�=qA�?}A�(�A�{A��A�|�A�%A���A���A�jA�7LA���A�9XA��FA��A���A�7LA���A�x�A��A�ZA�VA��A�(�A�"�A�bA�A��mA�A��A���A��+A�|�A�~�A��A��\A��DA�n�A�\)A�+A��FA�"�A��A�A�\)A�^5A��A��A�&�A�^5A��PA���A��A���A�ffA��A�`BA�XA���A�l�A�A� �A��/A��hA��!A��#A�\)A�(�A�A�A���A�A�n�A�VA�hsA���A��A�x�A�dZA�l�A��RA���A���A�r�A�G�A��A�=qA���A�|�A�33A�bNA��PA��wA���A�S�A��A��A��wA�|�A�VA�`BA���A}?}A{��Az�!Aw��Av�Au;dAsC�AqC�Ap1Ao\)An��Am�TAm
=Aj�Ag�
Ag33Af5?Ad�/Ac+A_�A\�+AZA�AWƨATAR��AO&�AKO�AI�PAG�AE�PAC�AA�wA@^5A>�!A;�-A:^5A8n�A6��A5�A4jA3��A2�jA1�A0I�A.�A.$�A-��A-&�A+��A+33A*Q�A(��A'hsA&ȴA&=qA%�-A$�DA$1A#��A#�A#`BA#/A"��A!O�A {A��AC�Ar�A�FA  A��A�/A�/Al�A��AG�A�HA�FA�`A�+A�A �Ax�AVA
�jA	�A=qA��A%AI�A�A^5A%A�A/@���@�"�@���@�v�@��j@��@�@�/@��y@�{@��#@�dZ@�@�@�t�@�^5@��@�bN@��@��@�-@�@���@��@��;@�+@�7L@߮@�=q@�M�@�@ܛ�@��@�%@�  @�ȴ@�~�@�?}@�|�@֧�@�J@�p�@��/@��
@�"�@�G�@�Z@�I�@� �@���@�;d@�$�@͉7@��@��@�S�@��/@�\)@�K�@�l�@Ƈ+@�@�j@�+@�5?@�`B@�Z@��@���@��@��j@���@�+@�@�G�@���@��@��;@��y@���@���@��@��@�(�@��;@���@��@���@���@�j@���@�|�@�;d@�ff@�J@�x�@��@���@��j@�r�@�+@��@�/@�?}@�Ĝ@��D@�Q�@�  @�t�@�~�@�M�@�V@��T@�1'@�S�@���@���@��@��!@���@�E�@��h@�V@��`@�Ĝ@���@�E�@���@��@�x�@���@��w@���@��P@�
=@�~�@�V@�E�@���@���@�ff@�^5@�5?@�E�@��\@��R@��@�\)@��
@��@�t�@�S�@�;d@���@��+@��@�$�@��@��@�X@�O�@���@�Q�@�ƨ@���@���@��@���@��h@�X@�?}@�/@���@���@���@�z�@�1'@� �@�1'@�I�@�/@��@��`@��D@�1'@�dZ@�dZ@���@���@�t�@�S�@�K�@�;d@�S�@�
=@���@�n�@�{@��T@��-@�x�@�X@�7L@���@�r�@���@���@�r�@�I�@�(�@��m@���@��P@�K�@�"�@��!@�~�@�ff@�E�@��@�{@�J@��#@��^@���@��h@�hs@�7L@���@���@�Q�@�9X@�b@�t�@���@���@��#@��@��-@��h@���@��@���@���@�`B@�X@���@�z�@�1@�\)@���@��@��@�~�@�{@��^@��T@�^5@��@���@��R@�+@�@���@���@�5?@�-@�5?@�v�@���@��@��#@���@��@Z�@u/@p4n@e�D@_$t@SiD@Ka@EG�@?��@9X@5��@1�@*��@%;@�@L0@i�@�h@��@	S&11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�+A�(�A�&�A�5?A�?}A�=qA�=qA�A�A�A�A�A�A�C�A�C�A�A�A�C�A�?}A�=qA�=qA�;dA�;dA�=qA�?}A�(�A�{A��A�|�A�%A���A���A�jA�7LA���A�9XA��FA��A���A�7LA���A�x�A��A�ZA�VA��A�(�A�"�A�bA�A��mA�A��A���A��+A�|�A�~�A��A��\A��DA�n�A�\)A�+A��FA�"�A��A�A�\)A�^5A��A��A�&�A�^5A��PA���A��A���A�ffA��A�`BA�XA���A�l�A�A� �A��/A��hA��!A��#A�\)A�(�A�A�A���A�A�n�A�VA�hsA���A��A�x�A�dZA�l�A��RA���A���A�r�A�G�A��A�=qA���A�|�A�33A�bNA��PA��wA���A�S�A��A��A��wA�|�A�VA�`BA���A}?}A{��Az�!Aw��Av�Au;dAsC�AqC�Ap1Ao\)An��Am�TAm
=Aj�Ag�
Ag33Af5?Ad�/Ac+A_�A\�+AZA�AWƨATAR��AO&�AKO�AI�PAG�AE�PAC�AA�wA@^5A>�!A;�-A:^5A8n�A6��A5�A4jA3��A2�jA1�A0I�A.�A.$�A-��A-&�A+��A+33A*Q�A(��A'hsA&ȴA&=qA%�-A$�DA$1A#��A#�A#`BA#/A"��A!O�A {A��AC�Ar�A�FA  A��A�/A�/Al�A��AG�A�HA�FA�`A�+A�A �Ax�AVA
�jA	�A=qA��A%AI�A�A^5A%A�A/@���@�"�@���@�v�@��j@��@�@�/@��y@�{@��#@�dZ@�@�@�t�@�^5@��@�bN@��@��@�-@�@���@��@��;@�+@�7L@߮@�=q@�M�@�@ܛ�@��@�%@�  @�ȴ@�~�@�?}@�|�@֧�@�J@�p�@��/@��
@�"�@�G�@�Z@�I�@� �@���@�;d@�$�@͉7@��@��@�S�@��/@�\)@�K�@�l�@Ƈ+@�@�j@�+@�5?@�`B@�Z@��@���@��@��j@���@�+@�@�G�@���@��@��;@��y@���@���@��@��@�(�@��;@���@��@���@���@�j@���@�|�@�;d@�ff@�J@�x�@��@���@��j@�r�@�+@��@�/@�?}@�Ĝ@��D@�Q�@�  @�t�@�~�@�M�@�V@��T@�1'@�S�@���@���@��@��!@���@�E�@��h@�V@��`@�Ĝ@���@�E�@���@��@�x�@���@��w@���@��P@�
=@�~�@�V@�E�@���@���@�ff@�^5@�5?@�E�@��\@��R@��@�\)@��
@��@�t�@�S�@�;d@���@��+@��@�$�@��@��@�X@�O�@���@�Q�@�ƨ@���@���@��@���@��h@�X@�?}@�/@���@���@���@�z�@�1'@� �@�1'@�I�@�/@��@��`@��D@�1'@�dZ@�dZ@���@���@�t�@�S�@�K�@�;d@�S�@�
=@���@�n�@�{@��T@��-@�x�@�X@�7L@���@�r�@���@���@�r�@�I�@�(�@��m@���@��P@�K�@�"�@��!@�~�@�ff@�E�@��@�{@�J@��#@��^@���@��h@�hs@�7L@���@���@�Q�@�9X@�b@�t�@���@���@��#@��@��-@��h@���@��@���@���@�`B@�X@���@�z�@�1@�\)@���@��@��@�~�@�{@��^@��T@�^5@��@���@��R@�+@�@���@���@�5?@�-@�5?@�v�@���@��@��#G�O�@��@Z�@u/@p4n@e�D@_$t@SiD@Ka@EG�@?��@9X@5��@1�@*��@%;@�@L0@i�@�h@��@	S&11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�'B
�-B
�9B
�-B
ÖB
��B
�)B
�B
�B
��B
��B
��B
��B
�B
�B
�B
��B
��BG�B�B�?B�`B�B��B%B�B"�B%�B,B2-B6FB8RB9XB:^B<jB@�BA�BE�BF�BG�BH�BK�BS�B_;BbNBgmBffBJ�B;dB;dB;dB&�B?}BK�BG�B9XB5?B2-B49B7LB2-B-B)�B#�B�B�B�B	7B��B��B�;B�B�;B��B�B��B��BŢB��B�LB��B��B�VB�JB�1B�Bs�BaHBVBO�BH�B5?BuB
�ZB
��B
ŢB
�}B
�dB
��B
z�B
~�B
l�B
bNB
H�B
6FB
1'B
&�B
oB
DB	��B	�B	�TB	�/B	�B	��B	ǮB	��B	�B	��B	��B	�oB	�B	m�B	Q�B	=qB	/B	\B	B�B�B��BB�dB�?B�!B�B��B��B��B��B��B�oB�bB�PB�JB�7B�+B�B�B�B~�B}�Bw�Bs�Bq�Bk�BiyBhsBp�Bp�Bm�B�B�B�B�B�B� Bz�Bq�Bl�BZBVBL�BI�BG�BC�B?}B9XB8RB9XB7LB9XB6FB6FB:^B9XB7LB7LB8RB8RB49B6FB49B1'B33B,B-B(�B(�B%�B'�B'�B%�B&�B%�B%�B(�B,B:^BG�BW
BXBS�BP�BM�BM�BO�BM�BT�BR�BO�BP�BP�BP�BT�BS�BR�BW
B\)BYB[#B]/B]/B\)B^5B]/B_;B^5B_;B^5B`BB`BB`BB`BB]/B]/B]/B^5B`BBgmBhsBhsBl�Bl�BiyBjBk�Bn�Bv�Bz�B�B�B�B�B�%B�=B�PB�\B�hB�{B��B��B��B��B��B��B��B��B��B�B�'B�!B�RB�XB�LB�dB�dB�wBÖBĜBȴB��B��B�)B�/B�5B�HB�ZB�NB�mB�B�B�B�B�B�B�B�B�B��B��B��B�B��B��B	
=B	\B	�B	�B	�B	�B	�B	 �B	$�B	#�B	"�B	%�B	'�B	'�B	'�B	+B	/B	1'B	1'B	2-B	49B	8RB	:^B	;dB	<jB	=qB	A�B	G�B	J�B	O�B	T�B	\)B	]/B	_;B	`BB	`BB	aHB	bNB	dZB	gmB	jB	k�B	m�B	p�B	p�B	q�B	q�B	p�B	q�B	s�B	s�B	t�B	u�B	v�B	v�B	x�B	z�B	}�B	�B	�B	�B	�1B	�=B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�9B	�?B	�FB	�RB	�dB	�wB	��B	B	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�
B	�B	�)B	�HB	�TB	�ZB	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B
�B
�B
�B
'8B
.IB
/�B
6�B
?}B
G�B
IlB
NVB
T,B
XB
[	B
a�B
ffB
k�B
o�B
s�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�&B
�B
��B
��B
�B
�lB
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B:�Bu�B�B�1B�tB�B��B	SB�B�B�B$�B)B+B,!B-'B/3B3KB4QB8jB9pB:vB;|B>�BF�BRBUBZ2BY+B=�B./B./B./B�B2HB>�B:yB,%B(B$�B'B*B$�B�B�B�B�B�BTB�B��B�B�B�iB�B��B��B»B��B�B�aB�+B��B�jB�:B.B{Bv�Bf�BT1BH�BB�B;�B(-BgB
�QB
��B
��B
�xB
�`B
��B
m�B
q�B
_�B
UUB
;�B
)RB
$4B
�B
B	�UB	�B	ޚB	�jB	�FB	�.B	��B	��B	��B	�+B	��B	��B	��B	x@B	`�B	EB	0�B	"GB	�B�PB��B�;B��B��B��B�zB�]B�EB�-B�B��B��B��B��B��B��B�B|zBznBvVBvVBtJBr?Bq9BkBf�Bd�B^�B\�B[�Bc�Bc�B`�BvXBuRBxeBxeBxeBsGBn(Bd�B_�BMiBIPB@B=B:�B6�B2�B,�B+�B,�B*�B,�B)�B)�B-�B,�B*�B*�B+�B+�B'�B)�B'�B${B&�B]B cBKBLB9BFBFB:B@B:B:BMB_B-�B;BJ]BKcBGKBD9BA'BA'BC3BA'BHRBFFBC4BD:BD:BD:BHSBGMBFGBJ_BO}BLlBNwBP�BP�BO~BQ�BP�BR�BQ�BR�BQ�BS�BS�BS�BS�BP�BP�BP�BQ�BS�BZ�B[�B[�B_�B_�B\�B]�B^�Ba�BjBn5BtYBu_BveBwlByxB}�B��B��B��B��B��B��B��B��B�B�B�B�B�B�YB�wB�rB��B��B��B��B��B��B��B��B�B�-B�LB�vB�|BтBԕBצB՛BڹB��B��B��B��B��B��B��B��B��B�B�B�B�B� B�,B��B	�B	�B	�B	�B	�B	B	B	%B	B	B	+B	7B	8B	8B	IB	"bB	$nB	$nB	%tB	'�B	+�B	-�B	.�B	/�B	0�B	4�B	:�B	>B	C#B	HAB	OlB	PqB	R}B	S�B	S�B	T�B	U�B	W�B	Z�B	]�B	^�B	`�B	c�B	c�B	d�B	d�B	c�B	d�B	f�B	f�B	g�B	iB	j
B	j
B	lB	n!B	q4B	tFB	wYB	x^B	{pB	}|B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�,B	�8B	�>B	�>B	�>B	�>B	�DB	�WB	�]B	�bB	�nB	�tB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�+B	�7B	�BB	�=B	�7B	�7B	�1B	�+B	�%B	�%B	�%B	�1B	�1B	�=B	�7B	�CB	�PB	�aB	ԀB	֌B	גB	ܰB	ݶB	ܰB	ܰB	ݶB	޼B	��B	��B	��B	��B	��G�O�B	��B	��B	��B
�B
kB
!|B
"�B
)�B
2�B
:�B
<�B
A�B
G]B
KAB
N:B
U0B
Y�B
_B
cB
gB
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.013(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144242022020411442420220204114424  AO  ARCAADJP                                                                    20200619170925    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170925  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170925  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114424  IP                  G�O�G�O�G�O�                