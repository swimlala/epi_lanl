CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:59Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170859  20220204114413  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؎��1   @؎>���@8}�-V�c�;dZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@���A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bg��Bo��Bx  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�  D�]�D���D���D���D�c�D��)D��\D�\D�R�D��
D���D�)D�L)Dڌ{D��{D��D�S�D�RD��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@��R@��A��A<��A\��A|��A�z�A�z�A�z�A�G�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB^�Bf�Bn�Bw=qB=qB���B���B�k�B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4�=D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIz=DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRz=DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt�=Dy��D��D�W�D��{D���D���D�]�D��D��HD�HD�L{D���D�ƹD�D�FDچgD��gD��D�M�D�>D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AҰ!Aҧ�AҸRAҼjAҾwA���A���A���A�A�A�A�A�ĜA�ƨA�ƨA�ƨA���A��/A��A�bA� �A��A�x�A���A��yA�ƨA�XA�7LAʬA���A��#A�C�AŸRA���A�E�A�ƨA�?}A��
A��wA��-A���A�"�A��7A�33A�
=A��yA��RA���A�XA� �A�?}A���A�A��A�x�A�C�A�  A��A���A���A�XA�{A�bA��;A��^A�oA��`A�/A�ƨA�5?A��HA�oA�{A���A���A��A�+A��\A�  A�p�A�VA�ĜA���A�9XA��A���A�jA�33A���A�A��A�1'A�ĜA�jA�{A���A�~�A�K�A���A���A��mA��RA���A�VA��9A��TA~v�Az�!Aw��Au�PArbNAn��Ak��Ah��AhVAg�7Ae�#Ad�\AdJAb �A_�A_33A`VA`E�A^=qA\�HAY�AWdZAU��AU+ATbNARv�AP�yAP$�AOK�AN=qAMAKt�AJJAI/AGl�AEoAD-ABJA@=qA>�!A=XA<�A<��A<bNA;�#A;�A:(�A9VA8(�A8{A7O�A4��A41A3�A2��A2��A2��A2�yA1��A/��A/x�A.�/A-��A-
=A,�+A+O�A*��A*ZA)��A'�TA'%A%&�A$A!�A VA�DA�
A�hAS�A�A�!AffA�AAE�Ax�A��AAȴA�wA�PAK�A�A��A�
A%A �AoA�AA�-AA�A�
A�7A
�A	�#A	p�A�AVAA��A�A��A^5A�A�7A�A ��@�C�@���@�%@���@��h@�&�@���@�ƨ@�@��@���@��@��@���@���@�`B@�I�@���@�\)@��y@��@���@�-@�7@��`@�C�@��/@��@��
@�dZ@�v�@ݩ�@�hs@݁@ݺ^@ݩ�@�G�@۶F@���@ڗ�@�M�@��@�
=@�ff@թ�@Դ9@�\)@ҟ�@�hs@мj@� �@�;d@�J@�%@�bN@�(�@��;@�l�@�
=@ʏ\@��T@ȃ@�K�@�5?@�hs@ě�@î@�C�@�@�v�@�@��j@�r�@�Z@�S�@�{@�&�@��@�Q�@���@�"�@�-@���@��m@�|�@��@��+@�5?@�@�/@��9@��D@�b@��@���@�~�@�-@�@��^@�x�@�`B@�X@�?}@�/@��@���@�I�@��;@�C�@�n�@���@���@�hs@�/@��@�bN@��w@�
=@��\@�7L@���@��@�t�@��@�+@��\@�&�@�?}@�%@��j@���@���@���@�
=@��@�I�@�(�@�b@��@��m@�ȴ@�/@�r�@�1'@�;d@�ƨ@��H@�M�@��@���@��^@���@���@���@���@��7@���@�A�@�1@��;@�t�@��@� �@��@���@��@�dZ@�@��!@�\)@�S�@�|�@�C�@���@��\@��h@���@�hs@��@��y@���@���@���@��+@�^5@�M�@��#@�`B@��@�z�@�b@��m@��m@�ƨ@��P@��w@���@��m@���@��@�\)@�@�z�@� �@��;@��F@��F@���@�"�@��@���@��R@�^5@��@���@�hs@�G�@�7L@�/@��@���@�bN@�j@�r�@�9X@���@��;@��
@���@�l�@��P@��F@���@��@�Q�@�A�@��@�1@�ƨ@���@�l�@�K�@�C�@�+@��@��@���@�ȴ@��+@�5?@�J@�J@�@���@���@�hs@�G�@�/@�%@�%@��@���@��9@���@�bN@���@|��@tr�@i0�@a�C@YIR@R��@G�@B$�@<֡@6Q@0�`@-+@'�F@#33@ @t�@Vm@e@_@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AҰ!Aҧ�AҸRAҼjAҾwA���A���A���A�A�A�A�A�ĜA�ƨA�ƨA�ƨA���A��/A��A�bA� �A��A�x�A���A��yA�ƨA�XA�7LAʬA���A��#A�C�AŸRA���A�E�A�ƨA�?}A��
A��wA��-A���A�"�A��7A�33A�
=A��yA��RA���A�XA� �A�?}A���A�A��A�x�A�C�A�  A��A���A���A�XA�{A�bA��;A��^A�oA��`A�/A�ƨA�5?A��HA�oA�{A���A���A��A�+A��\A�  A�p�A�VA�ĜA���A�9XA��A���A�jA�33A���A�A��A�1'A�ĜA�jA�{A���A�~�A�K�A���A���A��mA��RA���A�VA��9A��TA~v�Az�!Aw��Au�PArbNAn��Ak��Ah��AhVAg�7Ae�#Ad�\AdJAb �A_�A_33A`VA`E�A^=qA\�HAY�AWdZAU��AU+ATbNARv�AP�yAP$�AOK�AN=qAMAKt�AJJAI/AGl�AEoAD-ABJA@=qA>�!A=XA<�A<��A<bNA;�#A;�A:(�A9VA8(�A8{A7O�A4��A41A3�A2��A2��A2��A2�yA1��A/��A/x�A.�/A-��A-
=A,�+A+O�A*��A*ZA)��A'�TA'%A%&�A$A!�A VA�DA�
A�hAS�A�A�!AffA�AAE�Ax�A��AAȴA�wA�PAK�A�A��A�
A%A �AoA�AA�-AA�A�
A�7A
�A	�#A	p�A�AVAA��A�A��A^5A�A�7A�A ��@�C�@���@�%@���@��h@�&�@���@�ƨ@�@��@���@��@��@���@���@�`B@�I�@���@�\)@��y@��@���@�-@�7@��`@�C�@��/@��@��
@�dZ@�v�@ݩ�@�hs@݁@ݺ^@ݩ�@�G�@۶F@���@ڗ�@�M�@��@�
=@�ff@թ�@Դ9@�\)@ҟ�@�hs@мj@� �@�;d@�J@�%@�bN@�(�@��;@�l�@�
=@ʏ\@��T@ȃ@�K�@�5?@�hs@ě�@î@�C�@�@�v�@�@��j@�r�@�Z@�S�@�{@�&�@��@�Q�@���@�"�@�-@���@��m@�|�@��@��+@�5?@�@�/@��9@��D@�b@��@���@�~�@�-@�@��^@�x�@�`B@�X@�?}@�/@��@���@�I�@��;@�C�@�n�@���@���@�hs@�/@��@�bN@��w@�
=@��\@�7L@���@��@�t�@��@�+@��\@�&�@�?}@�%@��j@���@���@���@�
=@��@�I�@�(�@�b@��@��m@�ȴ@�/@�r�@�1'@�;d@�ƨ@��H@�M�@��@���@��^@���@���@���@���@��7@���@�A�@�1@��;@�t�@��@� �@��@���@��@�dZ@�@��!@�\)@�S�@�|�@�C�@���@��\@��h@���@�hs@��@��y@���@���@���@��+@�^5@�M�@��#@�`B@��@�z�@�b@��m@��m@�ƨ@��P@��w@���@��m@���@��@�\)@�@�z�@� �@��;@��F@��F@���@�"�@��@���@��R@�^5@��@���@�hs@�G�@�7L@�/@��@���@�bN@�j@�r�@�9X@���@��;@��
@���@�l�@��P@��F@���@��@�Q�@�A�@��@�1@�ƨ@���@�l�@�K�@�C�@�+@��@��@���@�ȴ@��+@�5?@�J@�J@�@���@���@�hs@�G�@�/@�%@�%@��@���@��9@���G�O�@���@|��@tr�@i0�@a�C@YIR@R��@G�@B$�@<֡@6Q@0�`@-+@'�F@#33@ @t�@Vm@e@_@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBǮBǮBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBǮBƨBȴBɺB��B�BB��B�B@�BP�BJ�BI�BM�BJ�BH�BK�BXBaHBe`Bn�Bt�B�bB�hB�\B�PB�%B� B}�B�%B�1B�B�B� B}�B� Bv�Bs�Bq�Bk�BjBe`BcTBcTBcTBcTBjBp�Bo�Bn�BffBcTBcTB`BBXBP�BM�BJ�B.B�BoB��B�B�BB��BȴB�dB�9B��B�JB|�Bq�B^5BG�B5?B�B�B\B
��B
�B
�B
�sB
�ZB
�#B
�
B
��B
��B
ȴB
��B
��B
�oB
�=B
�B
q�B
bNB
6FB
JB	�B	ǮB	��B	�bB	n�B	hsB	_;B	W
B	L�B	E�B	C�B	5?B	E�B	aHB	�1B	s�B	iyB	XB	A�B	1'B	)�B	&�B	�B	hB	DB		7B		7B	oB	{B	JB	%B��B�B�B�sB�)B��B��BǮBǮBȴBǮBĜB��B�dB�9B�-B�-B��B��B�uB�{B��B��B��B��B��B�oB�\B�=B�1B�1B�B�B�+B�By�Bq�Bk�BgmB]/BXBT�BS�BR�BR�BQ�BP�BP�BO�BN�BM�BL�BI�BI�BG�BE�BD�BC�BC�BB�BA�B?}B>wB<jB:^B8RB6FB5?B33B2-B2-B1'B0!B/B.B-B.B+B+B)�B(�B(�B'�B&�B%�B!�B �B �B�B�B �B#�B"�B"�B!�B �B�B�B �B�B�B#�B!�B!�B!�B#�B"�B"�B"�B"�B'�B+B,B.B33B6FB7LB8RB;dB=qB>wB;dB@�BD�BB�BG�BK�BK�BL�BM�BO�BP�BR�BR�BVBW
BXBXBXBW
BXBXB[#B\)B\)B]/B_;BaHBbNBdZBe`BgmBiyBk�Bq�Bp�Bq�Bs�Bw�Bx�Bz�B|�B}�B~�B�B�B�=B�PB�VB�bB�oB�{B��B��B��B��B��B�B�B�!B�-B�?B�qBŢBǮBɺB��B��B��B��B�B�#B�/B�NB�fB�mB�B�B�B��B��B	  B	B	+B	1B	PB	bB	hB	uB	�B	oB	�B	�B	�B	�B	�B	�B	�B	$�B	)�B	+B	)�B	+B	,B	+B	'�B	#�B	"�B	�B	�B	PB	hB	bB	uB	�B	�B	�B	�B	$�B	#�B	!�B	 �B	!�B	#�B	'�B	+B	/B	49B	9XB	;dB	:^B	;dB	?}B	D�B	I�B	R�B	R�B	S�B	S�B	T�B	W
B	[#B	]/B	bNB	bNB	cTB	e`B	hsB	k�B	m�B	q�B	t�B	w�B	z�B	� B	�B	�B	�B	�1B	�\B	�oB	��B	��B	��B	��B	��B	�bB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�FB	�XB	�dB	�dB	�qB	��B	B	B	ŢB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�)B	�/B	�5B	�5B	�BB	�HB	�NB	�TB	�NB	�TB	�TB	�ZB	�ZB	�`B	�fB	�fB	�mB	�yB	��B	�dB
�B
hB

B
"�B
(�B
4�B
;�B
@�B
F�B
L0B
P�B
W�B
\B
`\B
e�B
g�B
jB
o5B
s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B� B� B� B� B� B� B� B� B� B� B� B�B� B�B�B�DBיB�*B�B7�BH4BBBA
BE#BBB@BCBOaBX�B\�Be�BlB��B��B��B��B}vBwRBuFB}wB�B{kBxYBwSBuGBwSBnBk
Bh�Bb�Ba�B\�BZ�BZ�BZ�BZ�Ba�Bg�Bf�Be�B]�BZ�BZ�BW�BOhBH>BE,BBB%pBB	�B�TB��BפB�<B�B��B��B�B��BtXBiBU�B?B,�B)B�B�B
�[B
�+B
�B
��B
��B
ҙB
΁B
�oB
�EB
�,B
��B
�^B
��B
��B
x�B
i)B
Y�B
-�B
�B	�>B	�:B	��B	��B	f*B	`B	V�B	N�B	DbB	=8B	;,B	,�B	=8B	X�B	�B	kIB	aB	O�B	9 B	(�B	!�B	�B	;B		B	�B	 �B	 �B	
B	B	�B��B��B�IB�7B�B��BȇB�cB�QB�QB�WB�QB�?B�&B�B��B��B��B�wB�:B�B�"B�(B�4B�AB�MB�4B�B�B��B�B�B|�B{�B~�Bz�Bq�BiUBc1B_BT�BO�BL�BK�BJ�BJ�BI�BH�BH�BG�BF�BE�BD|BAjBAjB?^B=RB<LB;FB;FB:@B9:B7.B6(B4B2B0B-�B,�B*�B)�B)�B(�B'�B&�B%�B$�B%�B"�B"�B!�B �B �B�B�B�B�B{B{BtBtB{B�B�B�B�B{BuBuB|BoBoB�B�B�B�B�B�B�B�B�B�B"�B#�B%�B*�B-�B/B0	B3B5'B6-B3B89B<RB:EB?dBC}BC}BD�BE�BG�BH�BJ�BJ�BM�BN�BO�BO�BO�BN�BO�BO�BR�BS�BS�BT�BV�BX�BZB\B]B_#Ba/Bc:Bi_BhYBi_BkkBo�Bp�Br�Bt�Bu�Bv�Bx�B|�B��B�B�
B�B�#B�/B�5B�TB�kB�qB��B��B��B��B��B��B�#B�SB�_B�kB�xBńBǏB˨B��B��B��B��B�B�B�:B�MB�dB�qB�B��B��B��B��B	�B	B		B	"B	4B	
B	:B	@B	GB	:B	@B	@B	MB	�B	!�B	"�B	!�B	"�B	#�B	"�B	�B	�B	}B	eB	5B	�B		B	B	#B	5B	;B	SB	eB	�B	�B	xB	rB	xB	�B	�B	"�B	&�B	+�B	1B	3B	2
B	3B	7(B	<GB	AeB	J�B	J�B	K�B	K�B	L�B	N�B	R�B	T�B	Y�B	Y�B	Z�B	]	B	`B	c.B	e:B	iRB	ldB	owB	r�B	w�B	y�B	y�B	z�B	�B	�B	�B	�3B	�@B	�@B	�FB	�(B	�	B	�B	�	B	�B	�.B	�@B	�FB	�FB	�XB	�kB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�	B	�B	�'B	�3B	�3B	�FB	�XB	�eB	�vB	ǂB	ɏB	ʕB	̡B	ͧB	ϳB	кB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�G�O�B	�hB	�B	�/B
	B
�B
"B
 �B
,'B
3lB
8qB
>aB
C�B
HOB
O`B
S�B
W�B
]2B
_rB
a�B
f�B
j�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144132022020411441320220204114413  AO  ARCAADJP                                                                    20200619170859    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170859  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170859  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114413  IP                  G�O�G�O�G�O�                