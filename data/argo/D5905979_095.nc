CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:16Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170916  20220204114420  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               _A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���w؝�1   @���`�f@6�l�C��c���O�;1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    _A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B ffB  B  B  B   B(  B0  B8ffB>  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�HD�"�D�[�D��qD�ʏD�RD�H�D�� D��
D�\D�W
D���D���D� �D�Z=DڍD��D� �D�S�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@��@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�G�A�G�B=qB=qB=qB=qB'=qB/=qB7��B==qBG=qBO=qBW=qB_=qBg��Bo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]��C_�\Ca�\Cc��Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Dz=D��Ds�D��Ds�D��DmqD��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP�=DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt�=Dy�D��D�U�D��]D��{D�>D�B�D���D���D�HD�P�D���D���D��D�T)DڇD��qD��D�M�D� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��;A��HA��TA��TA��TA��TA��TA��TA��TA��HA��#A۲-A�7LA�A���A�|�A�9XA���A�z�A�S�A��A���A�%A�VA�33AƓuAŃA��A�&�A��A�7LA�9XA��RA��PA�l�A�dZA��PA���A�z�A��A�VA��A�bNA��jA��jA�K�A�1'A�&�A��A���A�p�A�G�A���A��A�
=A�`BA��DA���A�1A�
=A�C�A��A���A�+A��#A��/A���A��FA���A���A���A���A��A�r�A���A��7A�z�A�;dA���A��A�|�A�O�A��A�XA���A�VA���A���A���A��jA�ZA���A�hsA��A�S�A�z�A�O�A�ȴA}��A{p�Az-AyAv�+As%Ar1Aq�hAp��An^5Ak�mAj^5Ag�
Ag7LAf��Af(�Ad�A_p�A]�FA\�AZM�AXv�AW��AU��ATn�ATAS��AS7LAR~�AQO�AP�!AOVAM��AL��ALz�AK�mAJȴAH��AG��AE��AD�9AC�AC&�AA|�A@A?�A>�A=�A<bA:��A9�A9\)A8jA85?A7��A7"�A5oA37LA21A1��A0^5A.�/A,�jA,5?A*��A* �A(��A( �A&�+A%��A$��A$=qA"�A!�7A �/A�A�A�;A�-AC�A��Al�A��A��A�+A �AK�A�!A=qAt�A��A{AƨA�hA`BA?}A��A�uA�FA��A5?A\)A~�A��A33A9XA
�uA
{A�jA��A7LA��A�A�A�AO�AȴA�A �`@��+@�bN@�ff@�Ĝ@��;@��@��u@�^@�r�@��@�ff@�%@�bN@���@���@��/@��@�E�@��#@���@��H@��@߶F@�+@�+@�"�@�?}@�(�@ە�@��H@�^5@ٲ-@��m@ו�@ְ!@Չ7@�&�@���@ԋD@ӝ�@�-@�b@�n�@�{@�p�@�r�@˅@��@ɡ�@�`B@ț�@�\)@�J@�G�@ă@ÍP@�ff@�G�@��u@�@��@���@�^5@��@�X@��`@�Z@���@�=q@��@��@�I�@��@���@�M�@�-@�J@��#@��7@���@��@�A�@��@���@�C�@��!@��T@�x�@�r�@���@�ƨ@���@�+@���@�V@�@�?}@��D@�1'@��@�l�@�+@�M�@��#@��/@�I�@���@��H@�n�@���@�hs@���@�  @�S�@��@���@���@��\@�v�@�{@�O�@�&�@�%@���@�z�@�|�@�K�@�o@��+@�-@��#@�&�@��@�G�@�/@�Ĝ@�j@��
@���@���@�l�@�~�@�J@�n�@�M�@�X@�Ĝ@�I�@� �@��@�(�@�Z@�A�@��;@��@��;@�Q�@���@���@��`@��@�&�@�`B@��@��^@��-@���@���@�ƨ@�
=@���@���@�n�@�M�@�5?@�-@��@�M�@�V@�n�@��+@�{@��T@��-@��^@�p�@�Ĝ@��9@���@�Ĝ@�Ĝ@�Q�@���@�b@��;@���@��@��9@�ƨ@���@�%@��D@�?}@���@���@���@�@��@��h@��^@�@��@��9@��u@�9X@���@���@�|�@��w@��w@�l�@�ȴ@��+@�~�@�v�@�ff@�@��T@���@���@��@��@��`@���@�I�@�b@��w@��@���@�l�@�+@�
=@��@��R@��+@�$�@��T@�x�@�hs@�`B@�&�@��@��D@�9X@��@�  @��;@��F@���@�|�@�t�@�\)@�;d@�+@�"�@�"�@�
=@��!@��+@�~�@�Z@z�1@pS�@e�D@^Q@W��@R@M�@Ef�@?~�@9hs@3�@,�u@'�0@"�R@�t@�9@��@��@
�}@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A��;A��HA��TA��TA��TA��TA��TA��TA��TA��HA��#A۲-A�7LA�A���A�|�A�9XA���A�z�A�S�A��A���A�%A�VA�33AƓuAŃA��A�&�A��A�7LA�9XA��RA��PA�l�A�dZA��PA���A�z�A��A�VA��A�bNA��jA��jA�K�A�1'A�&�A��A���A�p�A�G�A���A��A�
=A�`BA��DA���A�1A�
=A�C�A��A���A�+A��#A��/A���A��FA���A���A���A���A��A�r�A���A��7A�z�A�;dA���A��A�|�A�O�A��A�XA���A�VA���A���A���A��jA�ZA���A�hsA��A�S�A�z�A�O�A�ȴA}��A{p�Az-AyAv�+As%Ar1Aq�hAp��An^5Ak�mAj^5Ag�
Ag7LAf��Af(�Ad�A_p�A]�FA\�AZM�AXv�AW��AU��ATn�ATAS��AS7LAR~�AQO�AP�!AOVAM��AL��ALz�AK�mAJȴAH��AG��AE��AD�9AC�AC&�AA|�A@A?�A>�A=�A<bA:��A9�A9\)A8jA85?A7��A7"�A5oA37LA21A1��A0^5A.�/A,�jA,5?A*��A* �A(��A( �A&�+A%��A$��A$=qA"�A!�7A �/A�A�A�;A�-AC�A��Al�A��A��A�+A �AK�A�!A=qAt�A��A{AƨA�hA`BA?}A��A�uA�FA��A5?A\)A~�A��A33A9XA
�uA
{A�jA��A7LA��A�A�A�AO�AȴA�A �`@��+@�bN@�ff@�Ĝ@��;@��@��u@�^@�r�@��@�ff@�%@�bN@���@���@��/@��@�E�@��#@���@��H@��@߶F@�+@�+@�"�@�?}@�(�@ە�@��H@�^5@ٲ-@��m@ו�@ְ!@Չ7@�&�@���@ԋD@ӝ�@�-@�b@�n�@�{@�p�@�r�@˅@��@ɡ�@�`B@ț�@�\)@�J@�G�@ă@ÍP@�ff@�G�@��u@�@��@���@�^5@��@�X@��`@�Z@���@�=q@��@��@�I�@��@���@�M�@�-@�J@��#@��7@���@��@�A�@��@���@�C�@��!@��T@�x�@�r�@���@�ƨ@���@�+@���@�V@�@�?}@��D@�1'@��@�l�@�+@�M�@��#@��/@�I�@���@��H@�n�@���@�hs@���@�  @�S�@��@���@���@��\@�v�@�{@�O�@�&�@�%@���@�z�@�|�@�K�@�o@��+@�-@��#@�&�@��@�G�@�/@�Ĝ@�j@��
@���@���@�l�@�~�@�J@�n�@�M�@�X@�Ĝ@�I�@� �@��@�(�@�Z@�A�@��;@��@��;@�Q�@���@���@��`@��@�&�@�`B@��@��^@��-@���@���@�ƨ@�
=@���@���@�n�@�M�@�5?@�-@��@�M�@�V@�n�@��+@�{@��T@��-@��^@�p�@�Ĝ@��9@���@�Ĝ@�Ĝ@�Q�@���@�b@��;@���@��@��9@�ƨ@���@�%@��D@�?}@���@���@���@�@��@��h@��^@�@��@��9@��u@�9X@���@���@�|�@��w@��w@�l�@�ȴ@��+@�~�@�v�@�ff@�@��T@���@���@��@��@��`@���@�I�@�b@��w@��@���@�l�@�+@�
=@��@��R@��+@�$�@��T@�x�@�hs@�`B@�&�@��@��D@�9X@��@�  @��;@��F@���@�|�@�t�@�\)@�;d@�+@�"�@�"�@�
=@��!@��+G�O�@�Z@z�1@pS�@e�D@^Q@W��@R@M�@Ef�@?~�@9hs@3�@,�u@'�0@"�R@�t@�9@��@��@
�}@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB%B%B%B%B%B%B%B+B+B+B+B+B1BVB"�B5?BM�B�DB��B�7B��B��B��BoBA�BiyB��B��B��B��B��B��B�uB��B�DBH�B_;Bn�B�B�B�%B�%Bz�Bq�Bp�Bk�BffBx�BdZB[#BcTBaHBk�Bv�Bp�Bk�Bn�B{�B|�Bt�B�B|�Bt�Bu�Br�BhsBQ�B@�B2-BbBBB��B��B�yB�B��B��B�B�BB�NB�HB�5B�)B�B��B�}B��Bv�BVB9XB%�B �B{BB
�yB
��B
�-B
��B
�PB
r�B
\)B
O�B
D�B
8RB
�B
\B

=B
B	��B	�;B	�
B	��B	ÖB	�}B	�^B	�!B	�uB	�1B	�B	u�B	m�B	e`B	\)B	VB	W
B	W
B	T�B	T�B	S�B	O�B	K�B	C�B	?}B	;dB	8RB	33B	+B	#�B	�B	�B	\B	VB	%B	  B��B��B�B�B�mB�TB�HB�/B�#B�B��B��B��B��B�jB�dB�3B�B��B��B��B��B�bB�1B�B� B|�Bz�Bv�Bt�Br�Bs�Bs�Bs�Br�Bp�Bn�Bl�BiyBhsBgmBe`BdZBbNBaHB_;B]/B\)B\)B[#BZBYBXBW
BVBQ�BQ�BO�BM�BK�BJ�BG�BH�BG�BF�BD�BE�BB�BA�B?}B>wB>wB;dB:^B5?B2-B1'B0!B/B-B/B.B,B+B+B+B+B,B)�B,B-B,B,B-B,B+B-B.B/B1'B6FB49B6FB6FB7LB8RB;dB:^B<jB=qB>wB>wB>wB@�BB�BB�B?}B>wB<jB:^B<jB=qB?}B@�BE�BH�BM�BO�BQ�BT�BXB\)B]/B`BBe`BhsBl�Bl�Bm�Bm�Bo�Br�Bs�Bt�Bu�Bx�B{�B}�B~�B~�B� B� B�B�B�1B�7B�=B�DB�JB�VB�oB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�9B�FB�jB�wBBƨBȴB��B��B��B�B�#B�/B�;B�BB�BB�HB�`B�B�B�B�B�B��B��B��B��B��B��B��B��B	B		7B	PB	bB	hB	�B	�B	 �B	"�B	"�B	&�B	.B	.B	0!B	/B	/B	33B	:^B	<jB	@�B	D�B	D�B	I�B	L�B	N�B	P�B	Q�B	T�B	W
B	YB	cTB	e`B	hsB	l�B	jB	e`B	cTB	aHB	aHB	e`B	gmB	jB	o�B	q�B	s�B	w�B	z�B	~�B	�B	�B	�B	�+B	�=B	�DB	�DB	�PB	�VB	�\B	�oB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�-B	�FB	�RB	�LB	�LB	�LB	�LB	�dB	�dB	�wB	B	ĜB	ĜB	ŢB	ƨB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�BB	�BB	�BB	�HB	�NB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	��B
-B
,B
SB
 B
'mB
-wB
5ZB
@�B
B�B
G�B
MPB
U2B
Z7B
^OB
e`B
j�B
m�B
q[B
v�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�`B
�`B
�`B
�`B
�`B
�fB�BB,qBEB�qB��B�dB��B��B��B	�B8�B`�B��B��B��B��B��B��B��B��B�fB?�BVaBe�By0B{=B}IB}IBrBh�Bg�Bb�B]�Bo�B[�BRKBZ|BXpBb�Bm�Bg�Bb�Be�BsBtBk�Bx-BtBk�Bl�Bi�B_�BIB7�B)[B�B�KB�>B�B��B�B�LB�(B�4B�EB�wBكB�}B�jB�^B�:B�
B��B��BnBMDB0�B&B	B�B
�LB
��B
�B
�zB
��B
��B
jB
S|B
G3B
;�B
/�B
�B
�B
�B	�mB	�+B	֘B	�hB	�&B	��B	��B	��B	��B	��B	�B	zwB	m*B	d�B	\�B	S�B	MmB	NsB	NsB	LhB	LhB	KbB	GIB	C2B	;B	6�B	2�B	/�B	*�B	"pB	EB	B	�B	�B	�B��B�rB�SB�AB�#B�B��B��BؽBԤBҘBЌB�tB�CB� B��B��B��B��B��B�pB�LB�3B�B��B�By�Bw~BtlBr_BnHBl;Bj/Bk5Bk5Bk5Bj0Bh$BfBdB`�B_�B^�B\�B[�BY�BX�BV�BT�BS�BS�BR�BQ�BP�BO�BN�BM�BIpBIpBGcBEWBCKBBFB?3B@9B?3B>.B<"B=(B:B9B7B5�B5�B2�B1�B,�B)�B(�B'�B&�B$�B&�B%�B#�B"�B"�B"�B"�B#�B!�B#�B$�B#�B#�B$�B#�B"�B$�B%�B&�B(�B-�B+�B-�B-�B.�B/�B2�B1�B3�B4�B6B6B6B8B:B:B7B6B3�B1�B3�B4�B7B8B=-B@?BE^BGjBIwBL�BO�BS�BT�BW�B\�B_�BdBdBeBeBg(Bj9Bk?BlEBmLBp^BspBu}Bv�Bv�Bw�Bw�By�B|�B�B��B��B��B��B��B��B��B�B�(B�.B�4B�:B�LB�RB�XB�pB��B��B��B��B��B��B��B��B��B�B�.B�9B�RB�XB�qB͉BҨBԳBֿB��B��B��B��B�	B�	B�B�B�!B�?B�?B�FB�RB�^B�dB�wB�}B��B	 �B	�B	�B	�B	B	-B	FB	RB	RB	iB	%�B	%�B	'�B	&�B	&�B	*�B	1�B	3�B	8B	<B	<B	A8B	DKB	FWB	HcB	IjB	L{B	N�B	P�B	Z�B	\�B	_�B	dB	a�B	\�B	Z�B	X�B	X�B	\�B	^�B	a�B	gB	i&B	k2B	oJB	r\B	vuB	x�B	y�B	{�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�1B	�JB	�DB	�7B	�7B	�7B	�PB	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�+B	�0B	�1B	�1B	�1B	�7B	�=B	�CB	�OB	�[B	�bB	�nB	�tB	΀B	φB	ЍB	ѓB	ѓB	ѓB	ҙB	ӟB	ԥB	ժB	׷B	׷B	׷B	ؽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� G�O�B	�kB	��B
�B
�B
�B
�B
$�B
,�B
8]B
:OB
?9B
D�B
L�B
Q�B
U�B
\�B
bXB
eB
h�B
nSB
r111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144202022020411442020220204114420  AO  ARCAADJP                                                                    20200619170916    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170916  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170916  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114420  IP                  G�O�G�O�G�O�                