CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-04-25T19:16:16Z AOML 3.0 creation; 2016-05-31T19:14:49Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7$   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7(   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7,   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7<   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7L   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7\   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7d   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8    DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8@   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8D   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8H   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8h   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
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
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20160425191616  20190604094001  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_146                   2C  D   APEX                            5368                            041511                          846 @קcm�F�1   @קdS��@3`ě��T�d1�"��`1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3Dy�fD�  D�FfD�y�D�i�D�	�D�6fD�y�D��fD�3D�FfD�� D�ɚD�fD�C3D�|�D�ɚD���D�9�D�vfD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @s�
@��@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�G�A�z�A�z�B=qB=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS��CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��DtG
Dy�=D���D�@RD�s�D�c�D��D�0RD�s�D��RD�D�@RD���D�ÆD� RD�=D�v�D�ÆD���D�3�D�pRD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A� �A� �A�"�A� �A��A�A�
=A�A�ȴA�+A���A�`BA��yA���A�ȴA�\)A�7LA�-Aǰ!AǁA�5?A�z�Aŏ\AĲ-A�S�A���A��#A�~�A��A+A��jA�\)A��HA���A��A��uA��A�hsA�-A�
=A���A��mA��FA��uA�jA��A���A�/A��wA�=qA���A��wA���A�A�\)A�jA���A��A�~�A��`A�1A��A��uA�7LA��
A��PA�jA�
=A�`BA�A�A���A��A�A�A�hsA�\)A�dZA�hsA��\A���A��mA�x�A� �A�?}A�A�1A�7LA��A�\)A�ƨA�A���A���A�/A���A���A���A��DA�ZA���A��-A�l�A��yA�%A�1'A�%A�hsA�t�A��DA��A~jA|ZAz��AyC�Av�AshsAq33AohsAm�AmG�AkS�Ai��Ahr�Ag�mAg�-Af�Af5?Ac�
Aa
=A^��A]+A\��A[/AX �AW?}AV��AU�;ASƨAQ��AM|�AI�AEhsAChsAB-A?�^A=�TA;?}A: �A9��A8v�A7x�A4��A2ĜA0�!A/�-A.�\A.A-%A+�FA*�`A*�DA*E�A)��A((�A$�`A#��A#��A"��A"(�A �`A jA �A�TA�A?}AA�`A�A�yA��Ap�A-Av�A�A��An�A��AQ�A�A��AS�A��A�A�TA+A�A�A�A`BA;dA
�yA	�TA-A�7A�\AbA��A��A�`A5?A?}A �!@���@�A�@�;d@�J@��@��@��@�"�@�?}@�Ĝ@�z�@�l�@�h@���@�~�@�=q@��`@�V@��#@���@�Ĝ@�9@��@�\@��#@�`B@��/@���@�R@�V@�+@��/@�dZ@ڗ�@���@�V@�bN@�|�@���@�@թ�@ա�@Ցh@���@��@�S�@҇+@�V@��@��H@́@���@��@�$�@���@��
@Ƨ�@���@��@�+@��-@�Z@�  @��F@��@���@��@�O�@�A�@���@��@���@�ff@�$�@��T@��h@��j@��H@���@��T@��@��#@��j@��F@�n�@��R@��T@�x�@��@��@���@���@��P@���@��^@�?}@��@��/@�Ĝ@�&�@�E�@���@���@�v�@�M�@��T@��@�/@��@��`@���@�ƨ@��@�C�@�-@�O�@�7L@��@���@��/@��D@��
@�ƨ@�t�@�\)@��H@�ff@��T@��7@�/@��9@�bN@���@��w@�|�@�"�@��H@��@��\@�$�@�@�X@�7L@��@���@��u@�r�@�Q�@� �@��w@��P@�l�@�S�@�+@��\@�=q@��@��@��h@�O�@��@��@���@�Ĝ@��9@�j@�9X@�1@���@�K�@�C�@�+@���@���@���@�M�@��T@��^@��7@�X@�/@��9@�(�@��w@���@�"�@���@���@���@�E�@�J@�@��^@�`B@�7L@�/@���@��@��j@�z�@�Z@�(�@��@��;@�t�@�\)@�
=@��@���@�ff@�V@�M�@�=q@�J@���@���@�p�@��@��9@��@� �@�33@�"�@��@���@�v�@�-@�{@��@�@�x�@�&�@���@���@��D@��9@��@��@�r�@�(�@��w@�l�@�@���@��\@�M�@�E�@�^5@�=q@��T@��^@���@��@��`@��@�b@�t�@�l�@��@��P@��P@�|�@�;d@��!@���@��T@�@���@�hs@��@�I�@�9X@�1@���@�$�@y�@p��@k@`Ĝ@Vv�@N��@H�`@A�#@;C�@2�H@-�T@*^5@$z�@+@^5@5?@G�@��@
-@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A� �A� �A�"�A� �A��A�A�
=A�A�ȴA�+A���A�`BA��yA���A�ȴA�\)A�7LA�-Aǰ!AǁA�5?A�z�Aŏ\AĲ-A�S�A���A��#A�~�A��A+A��jA�\)A��HA���A��A��uA��A�hsA�-A�
=A���A��mA��FA��uA�jA��A���A�/A��wA�=qA���A��wA���A�A�\)A�jA���A��A�~�A��`A�1A��A��uA�7LA��
A��PA�jA�
=A�`BA�A�A���A��A�A�A�hsA�\)A�dZA�hsA��\A���A��mA�x�A� �A�?}A�A�1A�7LA��A�\)A�ƨA�A���A���A�/A���A���A���A��DA�ZA���A��-A�l�A��yA�%A�1'A�%A�hsA�t�A��DA��A~jA|ZAz��AyC�Av�AshsAq33AohsAm�AmG�AkS�Ai��Ahr�Ag�mAg�-Af�Af5?Ac�
Aa
=A^��A]+A\��A[/AX �AW?}AV��AU�;ASƨAQ��AM|�AI�AEhsAChsAB-A?�^A=�TA;?}A: �A9��A8v�A7x�A4��A2ĜA0�!A/�-A.�\A.A-%A+�FA*�`A*�DA*E�A)��A((�A$�`A#��A#��A"��A"(�A �`A jA �A�TA�A?}AA�`A�A�yA��Ap�A-Av�A�A��An�A��AQ�A�A��AS�A��A�A�TA+A�A�A�A`BA;dA
�yA	�TA-A�7A�\AbA��A��A�`A5?A?}A �!@���@�A�@�;d@�J@��@��@��@�"�@�?}@�Ĝ@�z�@�l�@�h@���@�~�@�=q@��`@�V@��#@���@�Ĝ@�9@��@�\@��#@�`B@��/@���@�R@�V@�+@��/@�dZ@ڗ�@���@�V@�bN@�|�@���@�@թ�@ա�@Ցh@���@��@�S�@҇+@�V@��@��H@́@���@��@�$�@���@��
@Ƨ�@���@��@�+@��-@�Z@�  @��F@��@���@��@�O�@�A�@���@��@���@�ff@�$�@��T@��h@��j@��H@���@��T@��@��#@��j@��F@�n�@��R@��T@�x�@��@��@���@���@��P@���@��^@�?}@��@��/@�Ĝ@�&�@�E�@���@���@�v�@�M�@��T@��@�/@��@��`@���@�ƨ@��@�C�@�-@�O�@�7L@��@���@��/@��D@��
@�ƨ@�t�@�\)@��H@�ff@��T@��7@�/@��9@�bN@���@��w@�|�@�"�@��H@��@��\@�$�@�@�X@�7L@��@���@��u@�r�@�Q�@� �@��w@��P@�l�@�S�@�+@��\@�=q@��@��@��h@�O�@��@��@���@�Ĝ@��9@�j@�9X@�1@���@�K�@�C�@�+@���@���@���@�M�@��T@��^@��7@�X@�/@��9@�(�@��w@���@�"�@���@���@���@�E�@�J@�@��^@�`B@�7L@�/@���@��@��j@�z�@�Z@�(�@��@��;@�t�@�\)@�
=@��@���@�ff@�V@�M�@�=q@�J@���@���@�p�@��@��9@��@� �@�33@�"�@��@���@�v�@�-@�{@��@�@�x�@�&�@���@���@��D@��9@��@��@�r�@�(�@��w@�l�@�@���@��\@�M�@�E�@�^5@�=q@��T@��^@���@��@��`@��@�b@�t�@�l�@��@��P@��P@�|�@�;d@��!@���@��T@�@���@�hs@��@�I�@�9X@�1@���@�$�@y�@p��@k@`Ĝ@Vv�@N��@H�`@A�#@;C�@2�H@-�T@*^5@$z�@+@^5@5?@G�@��@
-@v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBx�Bx�Bx�Bx�Bx�Bx�Bx�Bw�Bw�Bv�Bs�Bs�By�B� B��B�}B��B��BɺB��B��B�BB�BB!�B8RB@�BE�BM�BZB]/BjBw�B{�B�B�B�B�B�B�%B�1B�=B�DB�JB�PB�\B�oB��B��B��B�B�9B�dB�jB�wBȴB��B�sB�B�B�B�B��B��B�`B�B��B�LB��B��B�NB�;B��B��B��B�LB��B��B�=B� Bl�BT�BH�B49B�B\BB�B��B�wB�3B��B�PBy�Bm�Bl�B[#BVB\)B^5B_;BXBM�BH�B49B#�B
��B
ȴB
�qB
�-B
��B
��B
�1B
x�B
m�B
_;B
K�B
5?B
%�B
�B
PB
%B	��B	�B	�ZB	�BB	�5B	�
B	��B	�}B	�B	��B	�hB	�DB	�B	p�B	jB	ffB	^5B	P�B	C�B	+B	�B��B�B�B�HB�B��B��B��BȴBĜB�qB�RB�3B�!B�B�B��B��B��B��B��B��B��B�oB�\B�VB�JB�=B�+B�%B�B�B�B�B~�By�Bx�Bw�Bu�Bs�Bp�Bo�Bn�Bm�Bl�Bk�Bk�Bk�Bk�Bl�Bl�BjBiyBiyBgmBffBjBl�BjBjBk�Bo�Bp�Br�Bq�Bq�Bo�Bn�Bk�BffBdZBdZBdZBdZBdZBcTBcTBcTBffBiyBjBiyBjBp�B~�B�DB�PB�JB�=B�JB�7B�=B�7B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�3B�9B�LB�^B�jB�}B�}B��BƨBȴB��B��B��B��B��B��B��B��B�)B�BB�HB�NB�HB�mB�B�B�B�B�B��B��B�B�B�B�B�B�B��B��B��B��B	%B	1B	+B	+B	+B	+B	%B	1B	
=B	
=B	
=B	JB	VB	\B	uB	�B	#�B	'�B	0!B	33B	5?B	6FB	8RB	;dB	=qB	=qB	?}B	?}B	A�B	F�B	K�B	L�B	L�B	M�B	N�B	P�B	W
B	XB	\)B	^5B	`BB	bNB	e`B	gmB	hsB	jB	k�B	n�B	n�B	p�B	t�B	u�B	w�B	z�B	|�B	~�B	�B	�B	�+B	�7B	�JB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�?B	�LB	�XB	�^B	�^B	�^B	�qB	�}B	��B	ÖB	ŢB	ŢB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�#B	�#B	�/B	�;B	�BB	�;B	�BB	�NB	�`B	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
JB
bB
!�B
(�B
0!B
5?B
?}B
C�B
I�B
P�B
XB
^5B
bNB
hsB
m�B
q�B
u�B
z�B
}�B
�B
�%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ByByByByByByBy
Bx
BxBwBs�Bs�BzB�6B�(B��B��B�B��B�!B�(B�{B��BDB"B8�B@�BE�BNBZTB]mBj�Bx
B|!B�MB�VB�QB�WB�YB�_B�iB�uB��B�B��B��B��B��B��B�B�>B�rB��B��B��B��B�4B�B��B��B��B��B�"B�B�B�UB�B��B�$B�0B�B�tB�8B�B��B��B�)B��B�yB�>Bl�BU:BH�B4sB�B�BWB��B�4B��B�nB�+B��BzBm�Bl�B[`BV=B\hB^kB_vBXLBNBH�B4uB$B
��B
��B
��B
�fB
�2B
��B
�kB
yB
m�B
_vB
L B
5|B
&B
�B
�B
bB	�B	��B	�B	��B	�qB	�GB	�B	��B	�@B	��B	��B	��B	�HB	p�B	j�B	f�B	^oB	Q#B	C�B	+;B	�B�-B��B��B�B�RB�.B�B�B��B��B��B��B�oB�^B�SB�FB�8B� B�B�B�B��B��B��B��B��B��B�{B�iB�eB�[B�YB�PB�EB8BzByBxBvBs�Bp�Bo�Bn�Bm�Bl�Bk�Bk�Bk�Bk�Bl�Bl�Bj�Bi�Bi�Bg�Bf�Bj�Bl�Bj�Bj�Bk�Bo�Bp�Br�Bq�Bq�Bo�Bn�Bk�Bf�Bd�Bd�Bd�Bd�Bd�Bc�Bc�Bc�Bf�Bi�Bj�Bi�Bj�Bp�B8B��B��B��B�{B��B�tB�|B�sB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�2B�LB�MB�RB�LB�aB�rB�{B��B��B��B��B��B��B��B��B�B� B�'B�7B�9B�1B�9B�7B�jB��B�B�B�B�B��B��B��B��B��B� B��B��B��B��B��B��B��B�B�B�B�!B	fB	sB	kB	oB	mB	hB	dB	qB	
|B	
�B	
{B	�B	�B	�B	�B	�B	$B	(3B	0cB	3pB	5�B	6�B	8�B	;�B	=�B	=�B	?�B	?�B	A�B	F�B	LB	MB	MB	NB	OB	Q$B	WJB	XOB	\hB	^uB	`�B	b�B	e�B	g�B	h�B	j�B	k�B	n�B	n�B	p�B	t�B	vB	xB	{!B	}-B	<B	�YB	�`B	�kB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�'B	�7B	�BB	�MB	�[B	�nB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�&B	�"B	�4B	�BB	�EB	�JB	�JB	�VB	�bB	�bB	�pB	�{B	��B	�|B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�(B	�,B	�/B	�0B	�'B	�'B	�"B	�B	�B	�B	�#B	�)B	�7B	�;B	�;B	�<B	�<B	�<B	�9B	�2B	�0B	�4B	�9B
 ?B
IB
IB
HB
EB
NB
PB
RB
TB
WB
LB
SB
WB
\B
VB
�B
�B
"
B
):B
0bB
5�B
?�B
C�B
I�B
Q&B
XOB
^yB
b�B
h�B
m�B
q�B
vB
{"B
~6B
�KB
�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0.0001), vertically averaged dS =0(+/-0.002) in PSS-78.                                                                                                                                                                                                 Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940012019060409400120190604094001  AO  ARCAADJP                                                                    20160425191616    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160425191616  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160425191616  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094001  IP                  G�O�G�O�G�O�                