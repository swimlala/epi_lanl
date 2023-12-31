CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:13:59Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141359  20220204114419  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               VA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�ɛ����1   @�ɜQ�{�@6c��$��c��t�j1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    VA   B   B   @333@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8ffB@  BG��BP  BW��B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D�3D�h�D���D��\D�!�D�\�D��)D���D��D�_\D���D��3D�$�D�T)DڕqD��fD�3D�VD��D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @'
=@s�
@��@��RA��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB�B=qB=qB'=qB/=qB7��B?=qBF�BO=qBV�B^�Bg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�B�k�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D�=Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��DtmqDy��D�D�b�D���D��HD��D�V�D��D���D��D�YHD���D��D��D�NDڏ]D��RD�D�P D�|�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A� �A�&�A�+A�+A�1'A�1'A�+A�-A�-A�/A�=qA�A�A�A�A�A�A�C�A�C�A�C�A�A�A�(�Aӝ�A���AѮA���A�/A�ZA��A���A��A�O�A�n�A��yA��Aú^A�Q�A�-A�-A��
A�XA�33A���A�\)A���A�I�A�?}A��mA�z�A��#A��\A�S�A��PA���A�%A�oA�hsA��A��A�;dA�r�A���A�JA��DA��!A�JA��!A�1A���A�A��A��A���A�l�A�oA��A��FA�bNA��-A���A���A�hsA��^A���A�\)A�G�A��A�5?A�(�A�7LA�hsA���A�ȴA�VA��HA�  A� �A�=qA���A���A�XA��A�^5A��-A�A�A���A���A��A��A�A}%A{x�Aw|�At��As�Ar=qAq�Ap��Ao33An  Ak�Af�\Ac�A`(�A\VAY�^AUAT��ATM�AQ�7AP~�AOAOp�AM�7AI�FAG"�AEK�ACC�AA�AB�uABĜAB5?AA�^A@�RA@A�A>��A;�mA:�9A9�A8n�A8E�A7�A7oA6^5A5t�A2��A1�FA0��A0��A0ZA0(�A/��A/�A.��A-VA+G�A*n�A)�^A)"�A(VA'��A&��A$��A$5?A#`BA"1A!
=A �RA A�A��AG�A�\A�;Ax�A�!A�hA1A(�A�AƨA9XA��A33Ar�A�TA��A&�AI�A��A�-AS�A��A�\A9XA�PA	�;AjA�A�AA �A�A�9Ar�A9XA��A��A|�A ��A $�@�9X@�ȴ@��h@���@��@��/@�t�@���@�@�A�@�C�@���@��@�\@���@�dZ@�X@�@��y@�5?@��u@�;d@ޏ\@�`B@�r�@�t�@ڰ!@���@�r�@ו�@�33@�\)@�@���@�ƨ@���@��y@�{@�?}@ԋD@�33@҇+@���@���@�S�@́@�V@��`@̛�@�r�@�(�@˅@ʇ+@ɉ7@�/@ț�@ǍP@�@�J@�?}@ģ�@�9X@��m@���@��@���@�z�@�1@���@�|�@�dZ@�C�@�"�@���@�M�@�{@��^@�`B@�X@�&�@���@���@���@��D@�r�@�C�@�33@�
=@��!@�/@��9@� �@���@���@���@�A�@��@�?}@��`@��@�j@�S�@��@��h@��@�p�@�/@��D@���@�dZ@���@��R@��\@�v�@�ff@�V@�J@��@��u@��@���@���@�l�@�"�@�~�@�7L@�Ĝ@��9@��`@��@���@�A�@�r�@�%@�^5@���@��#@��T@�{@�E�@���@�dZ@�dZ@�K�@�"�@��y@���@��+@�hs@�`B@�?}@���@���@��D@�9X@���@��@��R@�v�@��R@��R@���@�ff@�V@��@���@��@�p�@�?}@��/@��u@�r�@���@�ƨ@��@���@�S�@�K�@�;d@��y@��R@�v�@�V@�5?@��@���@��@�?}@���@�1'@��@�S�@�33@��@��H@��R@���@���@���@�~�@�@�x�@��@�Z@�(�@� �@�b@���@��
@��w@��@���@��+@�v�@��+@�^5@�5?@��@�1'@�9X@�j@�Q�@�(�@���@��+@��^@�@�@��7@�G�@�%@��D@��;@�ƨ@��@�;d@�;d@�;d@�@���@�ff@�{@���@��@��#@��^@�X@�%@��/@���@�bN@�Q�@�I�@�1'@��@�b@��m@�dZ@��@��!@�ff@�E�@�5?@�-@��T@���@��@�X@���@���@��D@�%�@|�.@u`B@lFt@c��@\b@R��@J�2@E��@<7�@5�j@2J�@-��@)*0@$�o@!4@�@�@O@n/@V�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A� �A�&�A�+A�+A�1'A�1'A�+A�-A�-A�/A�=qA�A�A�A�A�A�A�C�A�C�A�C�A�A�A�(�Aӝ�A���AѮA���A�/A�ZA��A���A��A�O�A�n�A��yA��Aú^A�Q�A�-A�-A��
A�XA�33A���A�\)A���A�I�A�?}A��mA�z�A��#A��\A�S�A��PA���A�%A�oA�hsA��A��A�;dA�r�A���A�JA��DA��!A�JA��!A�1A���A�A��A��A���A�l�A�oA��A��FA�bNA��-A���A���A�hsA��^A���A�\)A�G�A��A�5?A�(�A�7LA�hsA���A�ȴA�VA��HA�  A� �A�=qA���A���A�XA��A�^5A��-A�A�A���A���A��A��A�A}%A{x�Aw|�At��As�Ar=qAq�Ap��Ao33An  Ak�Af�\Ac�A`(�A\VAY�^AUAT��ATM�AQ�7AP~�AOAOp�AM�7AI�FAG"�AEK�ACC�AA�AB�uABĜAB5?AA�^A@�RA@A�A>��A;�mA:�9A9�A8n�A8E�A7�A7oA6^5A5t�A2��A1�FA0��A0��A0ZA0(�A/��A/�A.��A-VA+G�A*n�A)�^A)"�A(VA'��A&��A$��A$5?A#`BA"1A!
=A �RA A�A��AG�A�\A�;Ax�A�!A�hA1A(�A�AƨA9XA��A33Ar�A�TA��A&�AI�A��A�-AS�A��A�\A9XA�PA	�;AjA�A�AA �A�A�9Ar�A9XA��A��A|�A ��A $�@�9X@�ȴ@��h@���@��@��/@�t�@���@�@�A�@�C�@���@��@�\@���@�dZ@�X@�@��y@�5?@��u@�;d@ޏ\@�`B@�r�@�t�@ڰ!@���@�r�@ו�@�33@�\)@�@���@�ƨ@���@��y@�{@�?}@ԋD@�33@҇+@���@���@�S�@́@�V@��`@̛�@�r�@�(�@˅@ʇ+@ɉ7@�/@ț�@ǍP@�@�J@�?}@ģ�@�9X@��m@���@��@���@�z�@�1@���@�|�@�dZ@�C�@�"�@���@�M�@�{@��^@�`B@�X@�&�@���@���@���@��D@�r�@�C�@�33@�
=@��!@�/@��9@� �@���@���@���@�A�@��@�?}@��`@��@�j@�S�@��@��h@��@�p�@�/@��D@���@�dZ@���@��R@��\@�v�@�ff@�V@�J@��@��u@��@���@���@�l�@�"�@�~�@�7L@�Ĝ@��9@��`@��@���@�A�@�r�@�%@�^5@���@��#@��T@�{@�E�@���@�dZ@�dZ@�K�@�"�@��y@���@��+@�hs@�`B@�?}@���@���@��D@�9X@���@��@��R@�v�@��R@��R@���@�ff@�V@��@���@��@�p�@�?}@��/@��u@�r�@���@�ƨ@��@���@�S�@�K�@�;d@��y@��R@�v�@�V@�5?@��@���@��@�?}@���@�1'@��@�S�@�33@��@��H@��R@���@���@���@�~�@�@�x�@��@�Z@�(�@� �@�b@���@��
@��w@��@���@��+@�v�@��+@�^5@�5?@��@�1'@�9X@�j@�Q�@�(�@���@��+@��^@�@�@��7@�G�@�%@��D@��;@�ƨ@��@�;d@�;d@�;d@�@���@�ff@�{@���@��@��#@��^@�X@�%@��/@���@�bN@�Q�@�I�@�1'@��@�b@��m@�dZ@��@��!@�ff@�E�@�5?@�-@��T@���@��@�X@���@���G�O�@�%�@|�.@u`B@lFt@c��@\b@R��@J�2@E��@<7�@5�j@2J�@-��@)*0@$�o@!4@�@�@O@n/@V�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�jB	ĜB	�B
B
uB
'�B
D�B
Q�B
ZB
aHB
r�B
~�B
|�B
z�B
y�B
�%B
�B
�VB
��B
ĜB
��B
�BB
�BuB�B'�B0!B6FBVBq�B�B��B�B�jBŢB��B�TB��BB\B�B)�B+B+B;dB<jBA�BF�BH�BI�BI�BE�B �BB��B�B��B��B��B�JB�Br�B]/BF�B&�B
��B
��B
�'B
��B
��B
�=B
w�B
o�B
��B
��B
�{B
^5B
�B
B
A�B
�DB
z�B
[#B
E�B
9XB
.B
�B
VB	��B	�NB	��B	��B	ǮB	��B	�-B	��B	��B	u�B	XB	5?B	uB��B�`B�B�B�5B�#B�#B�B�B��B�}B�XB�9B�!B��B�HB�5B�#B�B�B��BɺBÖBB��B��B��B�jB�LB�3B�B��B��B��B��B��B��B��B��B��B��B�uB�hB�VB�JB�1B�+B�B|�Bx�Bt�Bq�Bo�Bo�Bm�Bm�Bk�BjBhsBffBcTB`BBZBYBS�BR�BN�BM�BL�BJ�BI�BF�BD�BB�BB�BB�B@�B@�B?}B=qB=qB9XB8RB8RB7LB6FB5?B49B33B2-B49B1'B1'B1'B33B0!B1'B/B0!B0!B/B/B/B/B/B/B33B33B1'B2-B33B2-B1'B1'B1'B1'B1'B1'B33B5?B5?B5?B5?B6FB7LB;dB<jB>wBE�BS�BXB^5BbNBdZBgmBl�Bm�Bp�Bu�Bu�By�B|�B}�B� B� B�B�+B�\B�hB�oB��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�-B�-B�-B�3B�FB�LB�XB�jB�}B��BB��B��B��B��B�B�B�;B�HB�NB�TB�HB�;B�B�B�B��B��B��B��BɺBɺB��B��B��B��B��B��B�
B�B�#B�5B�;B�HB�HB�TB�ZB�fB�sB�B�B��B��B��B	  B	B	%B	1B	PB	\B	hB	oB	�B	"�B	%�B	49B	7LB	7LB	8RB	;dB	=qB	B�B	K�B	M�B	L�B	M�B	M�B	O�B	P�B	Q�B	R�B	VB	ZB	ZB	ZB	]/B	_;B	^5B	bNB	bNB	ffB	iyB	k�B	k�B	k�B	m�B	p�B	r�B	r�B	s�B	t�B	u�B	w�B	}�B	� B	�B	�B	�B	�B	�B	�1B	�7B	�DB	�DB	�JB	�PB	�\B	�\B	�bB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�3B	�9B	�9B	�9B	�9B	�XB	�^B	�^B	�dB	�^B	�dB	�wB	�}B	��B	��B	ĜB	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�5B	�;B	�;B	��B	�B
KB
�B
B
)*B
5%B
:�B
=<B
@ B
D3B
KDB
Q B
SB
X�B
[=B
bNB
h�B
m]B
q�B
t9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	}{B	|uB	|uB	|uB	|uB	|uB	|uB	|uB	|uB	|uB	}zB	|uB	|uB	|uB	|uB	|uB	|uB	|uB	|uB	|uB	}{B	��B	��B	��B	��B	�gB
�B
 CB
<�B
J>B
RnB
Y�B
k B
wJB
u>B
s1B
r+B
~tB
}oB
��B
�>B
��B
�DB
؍B
� B�B�B 7B(hB.�BNIBi�B{TB��B�[B��B��B�*BۑB�B�[B�B�B"6B#<B#<B3�B4�B9�B>�B@�BA�BA�B=�BB�DB�,B�WB�B��B��B��ByMBj�BUxB>�B6B
�DB
�8B
�|B
�'B
��B
��B
p(B
g�B
�B
�GB
��B
V�B
B	�{B
9�B
��B
s=B
S�B
>B
1�B
&uB
B
�B	�FB	ڵB	�eB	�;B	�B	��B	��B	�aB	�B	n1B	P�B	-�B	�B�RB��BЉBҖB֯BӝBӝBҗBёB�BB��B��B��B��B�hB��BְBӟBѓBѓB�hB�7B�B�B�B�B�B��B��B��B��B�pB�dB�XB�RB�RB�XB�LB�FB�:B�B��B��B��B��B��B�Bz�BusBqZBmABj0Bh$Bh$BfBfBdBcB`�B^�B[�BX�BR�BQ�BL�BK{BGbBF\BEWBCKBBDB?2B='B;B;B;B9B9B8B5�B5�B1�B0�B0�B/�B.�B-�B,�B+�B*�B,�B)�B)�B)�B+�B(�B)�B'�B(�B(�B'�B'�B'�B'�B'�B'�B+�B+�B)�B*�B+�B*�B)�B)�B)�B)�B)�B)�B+�B-�B-�B-�B-�B.�B/�B3�B4�B7B>1BL�BP�BV�BZ�B\�B_�BeBfBi1BnPBnPBrhBu{Bv�Bx�Bx�By�B�B��B��B��B�B�=B�IB�PB�VB�VB�\B�bB�nB�hB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�VB�uB�{B́B΍BҦB��B��B��B��B��B��BҦBҦBѠB�iB�WB�LB�LB�EB�EB�LB�oB�vB�vB�|B̂BϔBҧBӭBֿB��B��B��B��B��B��B��B�'B�?B�RB�XB�^B��B��B��B	 �B	�B	�B		�B	
�B	B	WB	iB	,�B	/�B	/�B	0�B	3�B	5�B	;B	DKB	FWB	EQB	FWB	FWB	HcB	IiB	JpB	KvB	N�B	R�B	R�B	R�B	U�B	W�B	V�B	Z�B	Z�B	^�B	a�B	dB	dB	dB	fB	i&B	k2B	k2B	l8B	m>B	nEB	pQB	vuB	x�B	y�B	z�B	|�B	}�B	}�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�&B	�&B	�&B	�&B	�2B	�>B	�>B	�EB	�>B	�EB	�KB	�KB	�QB	�QB	�WB	�vB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�&B	�&B	�,B	�,B	�,B	�,B	�,B	�,B	�8B	�?B	�EB	�QB	�QB	�WB	�WB	�]B	�cB	�cB	�jB	�vB	ψB	ψB	ψB	ЎB	ѕB	ЎB	қB	ӡB	ԧB	ԧB	ֲB	׸G�O�B	�LB	�B
 �B
8B
�B
!�B
-�B
3AB
5�B
8zB
<�B
C�B
IzB
K�B
QB
S�B
Z�B
a:B
e�B
jqB
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144192022020411441920220204114419  AO  ARCAADJP                                                                    20200618141359    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141359  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141359  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114419  IP                  G�O�G�O�G�O�                