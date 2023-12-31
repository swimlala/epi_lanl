CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-06-09T17:03:06Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20170609170306  20190604094027  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�MhK��1   @�M�M@3�V��d�p��
=1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @���@���A   A@  A`  A�  A�  A�  A���A���A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3DyQHD��3D�@RD��{D��3D��D�3D���D�� D��D�IHD�y�D���D��D�_�D�}qD�ȤD��
D�H D�D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @s�
@��R@��RA��A<��A\��A|��A�z�A�z�A�G�A�G�A�z�A�z�A�z�A��B=qB=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS��CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��DtG
DyED��D�:>D��gD��D��D�D���D���D��D�C4D�s�D���D�qD�Y�D�w]D�D���D�A�D�z�D�s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aץ�A�JA���A��#Aӗ�A�|�A�XA�M�A�XA�Q�A�?}A�&�A�VA��mA�9XAʼjA��`A�=qA��A��`A�%A��TA��A¸RA�Q�A�A��A�O�A�A�A�VA�5?A��A��A���A�ffA�5?A��A���A�r�A�
=A�S�A��A�1A��9A��A���A��TA�M�A���A�^5A���A���A��A�7LA��mA�jA��RA��yA�ȴA�p�A��A��+A���A�oA�JA�C�A���A�$�A�E�A�dZA���A��/A�=qA���A�{A��A��9A�v�A�hsA�K�A�"�A�t�A��\A�/A�r�A��A�$�A��A���A�`BA�`BA��A��A�(�A�
=A��HA�z�A��\A��A��mA�{A��A��hA�\)A��A���A�+A}��Ax �Av�HAt=qAqx�Ap�!Ao�
AnQ�Ai|�Af�9Ae�^AdbAaC�A]�A\E�A[��AZ-AX�AXJAVffATVAR�RAQ�AP��AP�jAP��APr�AN�!ALI�AIoAE��AD�RAC��ABȴAA33A@�DA?�mA?oA>�uA>^5A>5?A=t�A;�wA:jA8�HA8Q�A7�A7ƨA5�A1��A/ƨA-ƨA,�/A,Q�A+�A*�yA*��A*~�A*^5A)��A(��A({A&A�A$�HA$�A$�A#|�A"VA"I�A!�
A!hsA!VA �jA Q�A�A�A�/A�yA��A��A��AbA�`A��A�A�hA�
A9XA��A�FA+A�A
�HA
=qA	%A��A�AhsA\)AG�A�A�AbNA�A7LA��A^5A��A�AĜAbA��AC�@��m@���@���@�33@�ȴ@��7@�`B@��D@��@��@��@���@���@��@��@���@�l�@�-@��/@�j@���@�\)@�E�@�
=@�D@�\@�^@�p�@�(�@�|�@���@⟾@ᙚ@���@�+@ޏ\@݁@ۮ@�{@�`B@أ�@��@��m@׾w@�t�@�C�@ְ!@�@�V@���@��`@�9X@�C�@��y@Η�@��@�X@��`@� �@�"�@�p�@�C�@Ƨ�@�v�@ũ�@å�@�C�@°!@���@�?}@���@��F@��;@��m@�+@���@�n�@�M�@��@��`@��@��F@��F@��@��!@�M�@�x�@��`@�Ĝ@�z�@�bN@���@�t�@��\@��@���@�x�@�?}@���@���@���@�/@��@�V@�V@��@��@��@���@�Ĝ@�r�@�A�@�r�@��@�r�@�I�@���@�K�@�o@���@���@���@�V@���@��/@��9@�Q�@��@���@���@�v�@�{@��@��h@�&�@��j@�I�@���@�|�@�dZ@�33@�o@�^5@�@��@�7L@���@���@���@��@�I�@�b@���@��P@�|�@�l�@�;d@�
=@��@���@�E�@��@��h@�X@�/@���@��/@��D@���@�+@���@�^5@�5?@��#@��7@�p�@�`B@���@�z�@�Q�@�Q�@�I�@�9X@�(�@�b@�  @���@��@�dZ@�S�@�C�@�33@�"�@�@��H@���@��!@��!@���@�5?@�$�@���@�?}@��@��D@�1'@�1@���@��
@�S�@��@�@��y@�ȴ@��R@���@���@��!@��+@�n�@�E�@��@���@��7@�G�@���@��`@��/@��/@���@���@���@��
@��;@���@��@�l�@�+@�
=@��@���@�$�@���@�@�7L@��j@��D@��@��w@��P@�\)@�C�@���@�ȴ@���@���@�ff@�5?@�J@���@��h@��7@�x�@�O�@��@��`@��j@��@���@���@{��@p�@c�@X�?@Q�C@JYK@F3�@@!@7j�@0�.@)ԕ@"�<@�@l�@U2@+@Y@�:@�{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Aץ�A�JA���A��#Aӗ�A�|�A�XA�M�A�XA�Q�A�?}A�&�A�VA��mA�9XAʼjA��`A�=qA��A��`A�%A��TA��A¸RA�Q�A�A��A�O�A�A�A�VA�5?A��A��A���A�ffA�5?A��A���A�r�A�
=A�S�A��A�1A��9A��A���A��TA�M�A���A�^5A���A���A��A�7LA��mA�jA��RA��yA�ȴA�p�A��A��+A���A�oA�JA�C�A���A�$�A�E�A�dZA���A��/A�=qA���A�{A��A��9A�v�A�hsA�K�A�"�A�t�A��\A�/A�r�A��A�$�A��A���A�`BA�`BA��A��A�(�A�
=A��HA�z�A��\A��A��mA�{A��A��hA�\)A��A���A�+A}��Ax �Av�HAt=qAqx�Ap�!Ao�
AnQ�Ai|�Af�9Ae�^AdbAaC�A]�A\E�A[��AZ-AX�AXJAVffATVAR�RAQ�AP��AP�jAP��APr�AN�!ALI�AIoAE��AD�RAC��ABȴAA33A@�DA?�mA?oA>�uA>^5A>5?A=t�A;�wA:jA8�HA8Q�A7�A7ƨA5�A1��A/ƨA-ƨA,�/A,Q�A+�A*�yA*��A*~�A*^5A)��A(��A({A&A�A$�HA$�A$�A#|�A"VA"I�A!�
A!hsA!VA �jA Q�A�A�A�/A�yA��A��A��AbA�`A��A�A�hA�
A9XA��A�FA+A�A
�HA
=qA	%A��A�AhsA\)AG�A�A�AbNA�A7LA��A^5A��A�AĜAbA��AC�@��m@���@���@�33@�ȴ@��7@�`B@��D@��@��@��@���@���@��@��@���@�l�@�-@��/@�j@���@�\)@�E�@�
=@�D@�\@�^@�p�@�(�@�|�@���@⟾@ᙚ@���@�+@ޏ\@݁@ۮ@�{@�`B@أ�@��@��m@׾w@�t�@�C�@ְ!@�@�V@���@��`@�9X@�C�@��y@Η�@��@�X@��`@� �@�"�@�p�@�C�@Ƨ�@�v�@ũ�@å�@�C�@°!@���@�?}@���@��F@��;@��m@�+@���@�n�@�M�@��@��`@��@��F@��F@��@��!@�M�@�x�@��`@�Ĝ@�z�@�bN@���@�t�@��\@��@���@�x�@�?}@���@���@���@�/@��@�V@�V@��@��@��@���@�Ĝ@�r�@�A�@�r�@��@�r�@�I�@���@�K�@�o@���@���@���@�V@���@��/@��9@�Q�@��@���@���@�v�@�{@��@��h@�&�@��j@�I�@���@�|�@�dZ@�33@�o@�^5@�@��@�7L@���@���@���@��@�I�@�b@���@��P@�|�@�l�@�;d@�
=@��@���@�E�@��@��h@�X@�/@���@��/@��D@���@�+@���@�^5@�5?@��#@��7@�p�@�`B@���@�z�@�Q�@�Q�@�I�@�9X@�(�@�b@�  @���@��@�dZ@�S�@�C�@�33@�"�@�@��H@���@��!@��!@���@�5?@�$�@���@�?}@��@��D@�1'@�1@���@��
@�S�@��@�@��y@�ȴ@��R@���@���@��!@��+@�n�@�E�@��@���@��7@�G�@���@��`@��/@��/@���@���@���@��
@��;@���@��@�l�@�+@�
=@��@���@�$�@���@�@�7L@��j@��D@��@��w@��P@�\)@�C�@���@�ȴ@���@���@�ff@�5?@�J@���@��h@��7@�x�@�O�@��@��`@��jG�O�@���@���@{��@p�@c�@X�?@Q�C@JYK@F3�@@!@7j�@0�.@)ԕ@"�<@�@l�@U2@+@Y@�:@�{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�PB��B�BB��B��BB+BbB�B �B!�B!�B �B�B33B`BBo�B|�B�B{�B{�Bz�Bz�Bz�By�Bz�B|�B� B�B�B�=B�=B�PB�uB��B��B��B��B��B��B�B�B�B�B�3B�9B�9B�9B�LB�FB�RB�}B��BÖBÖBÖB�}B�jB�jB�^B�LB�3B�B�B�B��B��B�BdZB:^B)�B�B�B	7BBB  B��B��B��B��B�B��B�^B�B��B�oB�VB�JB�Bt�BgmBR�BG�BE�BA�B49B-B!�BPBB
�B
��B
��B
}�B
p�B
`BB
D�B
)�B
 �B
oB
B	��B	��B	�B	��B	�dB	�9B	��B	��B	}�B	t�B	p�B	gmB	_;B	[#B	S�B	J�B	A�B	=qB	:^B	:^B	9XB	7LB	0!B	"�B	uB	B	B��B��B�B�B�yB�sB�mB�sB�sB�mB�`B�yB�yB�fB�ZB�BB��BB�jB�?B�-B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B�{B�{B�{B�uB�uB�oB�bB�PB�DB�+B�B|�Bw�Bs�Bq�Bo�Bn�Bm�BjBhsBhsBgmBgmBhsBgmBgmBffBffBgmBgmBgmBgmBffBffBffBffBe`Be`BffBe`Be`BdZBdZBcTBbNBbNBcTBdZBgmBhsBhsBiyBhsBhsBiyBhsBhsBhsBhsBhsBhsBgmBffBe`Be`BdZBcTBbNB_;B[#B[#BZB\)B]/B`BBbNBcTBcTBe`BiyBjBk�Bm�Bq�Bv�Bx�Bz�B|�B|�B}�B}�B}�B~�B� B� B�B�1B�1B�=B�DB�DB�JB�PB�VB�VB�\B�oB��B��B��B��B��B��B��B��B��B�B�3B�FB�^B��BÖBƨBȴB��B��B�
B�B�B�
B�B�)B�BB�NB�TB�ZB�ZB�`B�mB�B�B�B�B�B�B��B	B	%B	+B	1B	1B	
=B	DB	VB	bB	oB	�B	�B	$�B	(�B	,B	.B	/B	2-B	33B	33B	33B	6FB	9XB	9XB	9XB	:^B	<jB	=qB	?}B	D�B	H�B	L�B	M�B	P�B	S�B	W
B	\)B	bNB	ffB	hsB	jB	k�B	r�B	r�B	t�B	w�B	{�B	~�B	�B	�B	�+B	�7B	�7B	�DB	�JB	�JB	�VB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�!B	�'B	�'B	�-B	�3B	�9B	�FB	�RB	�RB	�XB	�XB	�XB	�^B	�^B	�dB	�jB	�jB	�jB	�jB	�jB	�qB	�qB	�}B	��B	ĜB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�;B	�;B	�BB	�NB	�TB	�TB	�ZB	�`B	�fB	�`B	�`B	�`B	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
tB
mB
"�B
-B
9	B
B[B
GEB
NB
QNB
U�B
\)B
`�B
fB
l�B
q�B
shB
wB
y�B
}�B
�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B��B��B׌B�B�5B�UB�tB�B�B	BBBB�B*wBW�Bf�Bt-BxJBs*Bs*Br$Br$Br"BqBr$Bt3BwABxIB|cB��B��B��B��B��B��B��B��B�
B�B�DB�SB�NB�XB�tB�B�yB�}B��B��B��B��B��B��B��B��B��B��B��B��B��B�tB�cB�bB�aB�8B��B{eB[�B1�B!IBB�B �B�eB�[B�OB�FB�CB�+B�B��B�B��B�qB��B��B��B��B{tBlB^�BJTB?B=B8�B+�B$qB0B�B
�sB
�B
�5B
�kB
udB
hB
W�B
<B
!tB
?B
	�B	��B	�nB	�GB	��B	�TB	��B	��B	�tB	�B	uzB	lDB	h,B	^�B	V�B	R�B	K�B	BKB	9B	4�B	1�B	1�B	0�B	.�B	'�B	_B	B��B��B�zB�^B�3B�B�B�B�B�	B�B�B��B�B�B��B��B��B̘B�(B�B��B��B��B��B��B��B��B��B��B��B�kB�RB�:B�0B�.B�B�B�B�B�B�B�B�B��B��B~�By�Bt�BooBkYBiOBgBBf9Be4Bb"B`B`B_B_B`B_B_B^	B^B_B_B_B_B^B^	B^B^B]B]B^B]B]B\ B\BZ�BY�BY�BZ�B\B_B`B`BaB`B`Ba B`B`B`B`B`B`B_B^B]	B]B[�BZ�BY�BV�BR�BR�BQ�BS�BT�BW�BY�BZ�BZ�B]	Ba%Bb)Bc)Be9BiRBnoBp}Br�Bt�Bt�Bu�Bu�Bu�Bv�Bw�Bw�B{�B�B�B��B��B��B��B��B��B��B�B�B�:B�@B�?B�FB�bB�mB�yB��B��B��B��B��B�B�+B�>B�NB�YB�pBȊBάBϳBϵBήB��B��B��B��B��B��B��B�B�B�#B�-B�;B�8B�;B�DB�mB��B��B��B��B��B	�B	�B	�B	B	
B	"B	KB	�B	 �B	#�B	%�B	&�B	)�B	*�B	*�B	*�B	-�B	0�B	0�B	0�B	1�B	4B	5B	7B	<7B	@SB	DkB	EsB	H�B	K�B	N�B	S�B	Y�B	]�B	`B	bB	c!B	jMB	jMB	lVB	omB	s�B	v�B	x�B	|�B	~�B	��B	��B	��B	��B	��B	��B	��B	� B	�	B	�B	�!B	�(B	�'B	�/B	�1B	�=B	�:B	�GB	�MB	�gB	�kB	�lB	�oB	�xB	�yB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�B	�
B	�B	�B	�2B	�<B	�DB	�MB	�gB	�oB	�zB	ʉB	̖B	͚B	ϣB	ЫB	ӻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�$B	�*B	�:B	�9B	�<B	�FB	�PB	�OB	�XB	�_B	�^B	�eB	�cB	�gB	�kB	�sB	�oB	�rB	�xB	�uB	�wB	�B	�B	�B	�B	��B	��B	��G�O�B	�	B
 B
HB
$�B
0�B
9�B
>�B
E�B
H�B
MLB
S�B
X;B
]�B
dOB
i�B
j�B
n�B
q6B
u�B
x�B
{(11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.008(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940272019060409402720190604094027  AO  ARCAADJP                                                                    20170609170306    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170609170306  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170609170306  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094027  IP                  G�O�G�O�G�O�                