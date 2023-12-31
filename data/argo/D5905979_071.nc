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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141355  20220204114418  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               GA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ض���Ǥ1   @ض�O��@6��-�c��
=p�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    GA   B   B   @�  @�  A   A   A@  Aa��A�  A���A�  A�33A�33A�  A���A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�
D��D�b=D���D��3D�${D�T�D��\D�ڏD��D�b�D���D��fD�\D�R=Dڏ\D��{D�)D�[�D�RD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��A��A<��A^�]A|��A�G�A�z�A��A��A�z�A�G�A��A�z�B=qB=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��{C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Dz=D�=Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D�qDs�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%mqD%��D&s�D&��D's�D'��D(s�D(�=D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRz=DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dpz=Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt�Dy��D��D�\)D���D��D�gD�N�D��HD��{D�qD�\{D��qD��RD�	HD�L)DډHD�~gD�D�UqD�>D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A¬A�n�A�t�A�l�A�Q�A�C�A�K�A�Q�A�S�A�"�A��A���A���A���A�  A�A�A���A��A��yA��yA��;A�ƨA��^A���A���A��7A���A��7A�t�A�dZA�oA���A�+A�A���A�I�A��yA�ȴA�+A�I�A��+A�JA���A�z�A�l�A�ffA��wA���A�$�A���A���A��hA�+A��PA���A�(�A��;A�n�A�=qA�A�`BA�l�A���A���A�%A��A��A�JA� �A��-A�A�A���A�+A��9A�?}A��A�oA�l�A���A��^A���A��PA��jA�p�A�ĜA���A���A���A���A�dZA�bA��7A��A�G�A�A�t�A�p�A�jA��RA��A�+A���A���A��A�&�A|�A~jA|r�Ay�7Aw��Au�Aq7LAo�AmXAi/Ae�Ad�+Ad�Ac��AcƨAcG�Aa�A_��A\��AZ  AV�ARA�AP(�AN�AMAL�+AJI�AGhsAE�AD  AC|�AB�/AB9XA@�A?�A>��A<��A:��A7G�A5|�A4bA1;dA/�mA.�A-�A,�/A*��A)+A(��A(Q�A'��A&�+A$�A"��A"^5A"=qA"$�A!��A A`BA��A�FA�A�FA��A1A�FA�A�A1A�hA%A�TAA��A�An�AA�AJA�FA�AJA��A7LA��A��AĜA{AƨA
��A
M�A	33AA�A��A�hA�/A�A �A��A�7A��AK�A�A33A ��@��P@��h@��`@�b@�@��T@�G�@��/@�
=@�`B@� �@�@�+@�@�F@�?}@��`@��`@�r�@���@�b@���@�n�@�(�@�`B@�A�@��@ᙚ@��D@��y@ݲ-@ۍP@��@٩�@�hs@��;@���@�=q@���@�l�@�33@�"�@���@�M�@���@��@���@�1@� �@�1'@�K�@�M�@���@̴9@�dZ@�ȴ@��@ɲ-@�`B@�O�@�X@�`B@�?}@���@�I�@�ȴ@�p�@Ĵ9@�1@ÍP@�;d@�E�@��@���@���@�A�@�dZ@�K�@�33@�ȴ@�@� �@�A�@��H@�n�@��h@�1@�ff@��@�l�@�S�@�+@��@��+@�J@�hs@�Q�@�b@��P@�@���@���@�9X@��@�V@���@�Ĝ@�j@� �@��m@�dZ@���@�{@�`B@��9@��@�^5@���@���@���@�33@���@��\@���@�X@�7L@���@�b@�"�@�
=@���@���@��!@�{@�x�@�%@��u@�I�@�b@���@�S�@��@��+@�=q@���@���@��u@�r�@�9X@��@���@�\)@�K�@�+@���@��R@��@��^@�x�@�O�@�/@�&�@��@�%@���@�r�@�j@�bN@�bN@�Z@�Z@�9X@�t�@�v�@�5?@��@�J@���@�X@��@�r�@��@���@��@�S�@��@��@�ȴ@��R@���@�V@�=q@�5?@�-@�{@���@�O�@�V@��@�  @��@�+@�ȴ@���@��+@�v�@�^5@�V@�5?@���@���@���@���@��h@�x�@�@��#@���@��j@��9@��@��@�%@���@��j@�j@�A�@�1@�\)@�S�@�+@��y@���@���@��\@��+@�v�@�V@�-@���@���@��@���@���@�?}@��9@��@��`@��@��@���@�  @��F@�l�@�33@���@���@�n�@�{@���@��h@��h@��7@���@���@�@��-@��7@��@���@��9@��D@�r�@�bN@�j@�A�@���@� �@yo @q�@gƨ@_�@W�@O��@I�@@Z@:��@3��@.�F@*=q@%��@'�@��@�N@A @�@U�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�A¬A�n�A�t�A�l�A�Q�A�C�A�K�A�Q�A�S�A�"�A��A���A���A���A�  A�A�A���A��A��yA��yA��;A�ƨA��^A���A���A��7A���A��7A�t�A�dZA�oA���A�+A�A���A�I�A��yA�ȴA�+A�I�A��+A�JA���A�z�A�l�A�ffA��wA���A�$�A���A���A��hA�+A��PA���A�(�A��;A�n�A�=qA�A�`BA�l�A���A���A�%A��A��A�JA� �A��-A�A�A���A�+A��9A�?}A��A�oA�l�A���A��^A���A��PA��jA�p�A�ĜA���A���A���A���A�dZA�bA��7A��A�G�A�A�t�A�p�A�jA��RA��A�+A���A���A��A�&�A|�A~jA|r�Ay�7Aw��Au�Aq7LAo�AmXAi/Ae�Ad�+Ad�Ac��AcƨAcG�Aa�A_��A\��AZ  AV�ARA�AP(�AN�AMAL�+AJI�AGhsAE�AD  AC|�AB�/AB9XA@�A?�A>��A<��A:��A7G�A5|�A4bA1;dA/�mA.�A-�A,�/A*��A)+A(��A(Q�A'��A&�+A$�A"��A"^5A"=qA"$�A!��A A`BA��A�FA�A�FA��A1A�FA�A�A1A�hA%A�TAA��A�An�AA�AJA�FA�AJA��A7LA��A��AĜA{AƨA
��A
M�A	33AA�A��A�hA�/A�A �A��A�7A��AK�A�A33A ��@��P@��h@��`@�b@�@��T@�G�@��/@�
=@�`B@� �@�@�+@�@�F@�?}@��`@��`@�r�@���@�b@���@�n�@�(�@�`B@�A�@��@ᙚ@��D@��y@ݲ-@ۍP@��@٩�@�hs@��;@���@�=q@���@�l�@�33@�"�@���@�M�@���@��@���@�1@� �@�1'@�K�@�M�@���@̴9@�dZ@�ȴ@��@ɲ-@�`B@�O�@�X@�`B@�?}@���@�I�@�ȴ@�p�@Ĵ9@�1@ÍP@�;d@�E�@��@���@���@�A�@�dZ@�K�@�33@�ȴ@�@� �@�A�@��H@�n�@��h@�1@�ff@��@�l�@�S�@�+@��@��+@�J@�hs@�Q�@�b@��P@�@���@���@�9X@��@�V@���@�Ĝ@�j@� �@��m@�dZ@���@�{@�`B@��9@��@�^5@���@���@���@�33@���@��\@���@�X@�7L@���@�b@�"�@�
=@���@���@��!@�{@�x�@�%@��u@�I�@�b@���@�S�@��@��+@�=q@���@���@��u@�r�@�9X@��@���@�\)@�K�@�+@���@��R@��@��^@�x�@�O�@�/@�&�@��@�%@���@�r�@�j@�bN@�bN@�Z@�Z@�9X@�t�@�v�@�5?@��@�J@���@�X@��@�r�@��@���@��@�S�@��@��@�ȴ@��R@���@�V@�=q@�5?@�-@�{@���@�O�@�V@��@�  @��@�+@�ȴ@���@��+@�v�@�^5@�V@�5?@���@���@���@���@��h@�x�@�@��#@���@��j@��9@��@��@�%@���@��j@�j@�A�@�1@�\)@�S�@�+@��y@���@���@��\@��+@�v�@�V@�-@���@���@��@���@���@�?}@��9@��@��`@��@��@���@�  @��F@�l�@�33@���@���@�n�@�{@���@��h@��h@��7@���@���@�@��-@��7@��@���@��9@��D@�r�@�bN@�j@�A�G�O�@� �@yo @q�@gƨ@_�@W�@O��@I�@@Z@:��@3��@.�F@*=q@%��@'�@��@�N@A @�@U�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB2-B2-B33B33B33B33B49B49B49B33B5?B49B5?B49B33B49B5?B5?B5?B6FB7LB7LB8RB8RB7LB7LB6FB49B5?B49B2-B1'B'�B�B�BVBhB�B"�B%�B:^BXBs�B�B�PB�{B�B�wBɺB��B��B�`B�B�B�B��BBBBB
=BJBJB{B�B�B'�B �B�B�B�B�B�BJBbBhBhB	7B��B�`B�/B�#B�B�B��B�9B��Bn�BT�B5?BbB
�TB
�wB
��B
��B
��B
�uB
�=B
z�B
k�B
cTB
S�B
K�B
?}B
,B
&�B
 �B
�B
{B
	7B	��B	�`B	�B	��B	�B	��B	�oB	y�B	jB	cTB	aHB	^5B	ZB	J�B	B�B	+B	!�B	PB��B�B�mB�ZB�BB�BĜB�RB�B��B��B��B��B��B��B�PB�B{�Br�Bs�Bm�Bm�Bk�BffBgmBe`BdZBdZBcTBaHB`BB\)B\)BZBZBZB\)BYBVBS�BR�BP�BP�BO�BN�BN�BM�BN�BN�BL�BK�BM�BK�BJ�BK�BI�BH�BH�BG�BG�BE�BB�BB�BA�BA�B?}B?}B>wB?}B=qB=qB;dB:^B:^B9XB9XB9XB:^B9XB9XB;dB<jB@�B?}B@�B?}B?}B?}BA�BA�BA�B@�BA�B>wB<jB:^B:^B:^B=qB@�BD�BF�BJ�BP�BQ�BQ�BS�B\)BT�BQ�BP�BQ�BR�BW
BXB]/B`BB`BB`BBffBgmBjBk�Bk�BjBiyBgmBffBffBhsBk�Bs�Bw�B{�B�B�7B�VB�oB�{B�{B�uB�oB�uB��B��B��B��B��B��B�B�-B�9B�FB�LB�LB�dB��BÖBÖBĜBȴBɺB��B��B��B��B��B�B�)B�;B�)B�)B�HB�`B�`B�ZB�ZB�TB�B�B�B�B�B�B�B�B�B�B	B	%B	B	B	B	B	  B��B��B��B��B��B�B��B	  B	B	B	+B		7B	bB	hB	oB	uB	�B	 �B	!�B	!�B	!�B	#�B	'�B	,B	0!B	33B	5?B	6FB	8RB	;dB	?}B	@�B	B�B	G�B	M�B	N�B	O�B	P�B	R�B	T�B	W
B	W
B	XB	[#B	^5B	bNB	cTB	e`B	gmB	iyB	jB	k�B	o�B	t�B	v�B	v�B	v�B	w�B	w�B	w�B	y�B	�B	�B	�%B	�%B	�%B	�+B	�7B	�=B	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�'B	�-B	�-B	�3B	�9B	�9B	�9B	�9B	�9B	�?B	�FB	�RB	�dB	�dB	�^B	�^B	�qB	��B	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ƨB	ƨB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�/B	�/B	�/B	�/B	�;B	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�B	�B
�B
9B
 �B
)�B
1�B
7�B
A;B
F�B
L~B
Q4B
TFB
Y�B
aHB
g8B
lWB
p�B
v+B
wB
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B*qB*qB+wB+wB+wB+wB,}B,}B,}B+wB-�B,}B-�B,}B+wB,}B-�B-�B-�B.�B/�B/�B0�B0�B/�B/�B.�B,}B-�B,}B*qB)kB 4B�B�B�B	�B�BB(B2�BPRBk�B}_B��B��B�LB��B��B�B�4BݛB��B��B��B�!B�SB�YB�@B�FBwB�B�B�B�B�B )B�B�B�B�B�B�B�B�B	�B	�BsB�7BݟB�oB�cB�]B�WB�B�|B��Bf�BMGB-�B�B
ۦB
��B
�RB
�"B
��B
��B
��B
s;B
c�B
[�B
LTB
D$B
7�B
$gB
IB
%B
B
�B
�B	�&B	��B	҂B	��B	��B	�RB	��B	rHB	b�B	[�B	Y�B	V�B	R�B	C1B	:�B	#tB	>B	�B�]B�B��B��BغBЉB�B��B��B�yB�mB�TB�<B�B��B��B|�BthBk1Bl8BfBfBdB^�B_�B]�B\�B\�B[�BY�BX�BT�BT�BR�BR�BR�BT�BQ�BN�BL~BKxBIkBIlBHfBG`BG`BFZBG`BG`BETBDNBF[BDOBCIBDOBBBBA<BA<B@6B@6B>+B;B;B:B:B8B8B7B8B5�B5�B3�B2�B2�B1�B1�B1�B2�B1�B1�B3�B4�B9B8B9B8B8B8B:B:B:B9B:B7B4�B2�B2�B2�B5�B9B=(B?4BCMBIqBJxBJxBL�BT�BM�BJxBIqBJxBK~BO�BP�BU�BX�BX�BX�B^�B_�BcBdBdBcBbB_�B^�B^�B`�BdBlBBpZBtrB}�B��B��B��B�B�B��B��B��B�$B�0B�6B�ZB�rB�B��B��B��B��B��B��B��B�B�B�B�$B�<B�BB�IB�[B�UB�aB�B΋B԰B��B԰B԰B��B��B��B��B��B��B�B�*B�B�6B�0B�%B�%B�B�B�<B��B��B��B��B��B��B��B�zB�tB�aB�UB�JB�=B�bB��B��B��B��B	�B	�B		�B	
�B	�B	%B	JB	PB	PB	PB	\B	 tB	$�B	(�B	+�B	-�B	.�B	0�B	3�B	8 B	9B	;B	@1B	FVB	G\B	HbB	IhB	KtB	M�B	O�B	O�B	P�B	S�B	V�B	Z�B	[�B	]�B	_�B	a�B	c B	dB	hB	m=B	oJB	oJB	oJB	pPB	pPB	pPB	r[B	y�B	}�B	~�B	~�B	~�B	�B	��B	��B	��B	��B	��B	� B	�B	�B	�+B	�+B	�1B	�1B	�=B	�DB	�DB	�DB	�JB	�PB	�gB	�nB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�1B	�7B	�7B	�7B	�7B	�>B	�DB	�DB	�JB	�UB	�UB	�[B	�[B	�aB	�hB	�tB	΀B	φB	ЌB	ѓB	ѓB	ѓB	ҙB	ӟB	ӟB	ӟB	ӟB	ӟB	իB	իB	իB	իB	׷B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	� B	��B
 �B
�B
	B
"B
*=B
0bB
9�B
?B
D�B
I�B
L�B
RB
Y�B
_�B
d�B
i5B
n�B
o�B
t)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144182022020411441820220204114418  AO  ARCAADJP                                                                    20200618141355    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141355  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141355  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114418  IP                  G�O�G�O�G�O�                