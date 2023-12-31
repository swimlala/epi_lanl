CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:56Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121041156  20190604094025  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @��|�D1   @��#�l@48�t�j�d�^5?}1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C ��C!��C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D��D�33D�` D��HD��D�O�D���D�ۅD�=D�9HD���D���D��D�H�Dڇ�D���D�
D�E�D�qHD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C ��C!��C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D��D�33D�` D��HD��D�O�D���D�ۅD�=D�9HD���D���D��D�H�Dڇ�D���D�
D�E�D�qHD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��
A���A���A���A���A���A�ĜA�ƨA�A���A��/A��`A��TA��;A��TA��HA��/A���AɼjAɲ-AɬAɬAɝ�Aɕ�AɓuAɍPAɉ7Aɉ7Aɇ+AɅAɃA�|�A�l�A�`BA�`BA�S�A�I�A�;dA�+A�+A�?}A�C�A�C�A�VA�jAɁAɏ\Aɕ�Aə�Aɣ�Aɗ�AɓuAɋDAɃAɇ+Aɇ+Aɇ+A�~�A�r�A�jA�bNA�^5A�VA�K�A�"�A��AȶFAȕ�A�r�A�(�AǶFA�;dAƙ�A�K�Ař�A�VAĮA�^5A��yA�oA� �A�p�A�z�A�t�A��A�C�A���A��PA���A�ffA��A�A�A�jA�1A���A��FA���A�VA�(�A�"�A��#A��`A�r�A���A��-A�33A���A��+A��\A�C�A�+A�A��RA�~�A�
=A��A�&�A�|�A���A��A�C�A���A�r�A���A�ĜA��A���A��mA�r�A�7LA��A��A��`A�1'A��A�
=A��+A�JA��A�dZA�9XA�  A��/A���A��mA|�yAz��Ax�uAu��ApffAmAj�RAh5?Af�Adr�Ab��Aa��A`�uA`jA^��A\ZAVA�AT��AT  ASoAP��AMoAKoAI�AIXAH�HAF��AD-AB��A@�jA?�hA>ZA<5?A:^5A9A97LA8n�A7�A6 �A2��A1��A1�A/"�A.�\A,�jA+33A*  A)VA(JA'XA&��A%�7A$ZA#;dA!t�A ��A ��A �A�wAx�A�/A�A��A�wA|�A�A(�A�A%A�A�/A��A �AoA��AĜA�jA��Av�A=qA�7AoA�\AhsA	`BA	p�A�+At�A1'A�
A�9A��A&�AI�A�PA ��A =qA   @��@��@��@���@�^5@���@�K�@�~�@��^@�7L@���@�@� �@��
@�@�@�r�@�hs@�C�@�9@���@�%@�@��@�7@��m@�C�@ޟ�@�n�@�5?@�{@ݲ-@ܣ�@�j@��@�C�@١�@�Q�@�|�@���@Ұ!@Ь@θR@��/@˝�@�o@ʏ\@�ff@�5?@�@ə�@Ɂ@�p�@�/@�Ĝ@�bN@�  @Ə\@��;@�+@�p�@� �@���@�
=@��+@�-@���@���@���@��@��@�O�@��@���@�^5@��@���@��y@���@�$�@���@��h@�`B@�V@���@��@��\@�-@�-@�J@��#@��@�G�@�%@��9@��@�bN@�1'@��@��w@��@��H@�ȴ@���@�V@�p�@��@��9@�z�@�I�@���@�K�@�;d@�+@���@���@���@�7L@�&�@��@��u@��;@��@��@�o@��R@�V@�p�@�X@�x�@��@�p�@��`@��@�n�@���@��`@���@�bN@��@��@��w@��@��@���@���@��P@�S�@���@�
=@���@��y@�@�o@�"�@�;d@�S�@�;d@��y@�ȴ@�ȴ@���@�ȴ@���@��!@��\@�n�@�V@�=q@�{@��#@���@�hs@�7L@�V@���@�Ĝ@��j@��@��@�  @�|�@�S�@�C�@�
=@��@���@�E�@�@�G�@�?}@���@��m@��P@��@��P@�|�@�+@���@��@�`B@�G�@��@�%@��/@���@��9@���@�bN@�I�@� �@�1@�  @��@��@�K�@���@�^5@�=q@�$�@�J@���@���@�p�@�&�@��D@�(�@�b@�  @��@���@��@��@�+@��y@���@�V@�$�@���@�x�@�%@���@�r�@�9X@��
@�g8@��@|bN@t�@m/@i&�@]O�@W�P@R��@G��@=L�@;\)@7��@.�@)!�@#��@��@&�@8�@�@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��
A���A���A���A���A���A�ĜA�ƨA�A���A��/A��`A��TA��;A��TA��HA��/A���AɼjAɲ-AɬAɬAɝ�Aɕ�AɓuAɍPAɉ7Aɉ7Aɇ+AɅAɃA�|�A�l�A�`BA�`BA�S�A�I�A�;dA�+A�+A�?}A�C�A�C�A�VA�jAɁAɏ\Aɕ�Aə�Aɣ�Aɗ�AɓuAɋDAɃAɇ+Aɇ+Aɇ+A�~�A�r�A�jA�bNA�^5A�VA�K�A�"�A��AȶFAȕ�A�r�A�(�AǶFA�;dAƙ�A�K�Ař�A�VAĮA�^5A��yA�oA� �A�p�A�z�A�t�A��A�C�A���A��PA���A�ffA��A�A�A�jA�1A���A��FA���A�VA�(�A�"�A��#A��`A�r�A���A��-A�33A���A��+A��\A�C�A�+A�A��RA�~�A�
=A��A�&�A�|�A���A��A�C�A���A�r�A���A�ĜA��A���A��mA�r�A�7LA��A��A��`A�1'A��A�
=A��+A�JA��A�dZA�9XA�  A��/A���A��mA|�yAz��Ax�uAu��ApffAmAj�RAh5?Af�Adr�Ab��Aa��A`�uA`jA^��A\ZAVA�AT��AT  ASoAP��AMoAKoAI�AIXAH�HAF��AD-AB��A@�jA?�hA>ZA<5?A:^5A9A97LA8n�A7�A6 �A2��A1��A1�A/"�A.�\A,�jA+33A*  A)VA(JA'XA&��A%�7A$ZA#;dA!t�A ��A ��A �A�wAx�A�/A�A��A�wA|�A�A(�A�A%A�A�/A��A �AoA��AĜA�jA��Av�A=qA�7AoA�\AhsA	`BA	p�A�+At�A1'A�
A�9A��A&�AI�A�PA ��A =qA   @��@��@��@���@�^5@���@�K�@�~�@��^@�7L@���@�@� �@��
@�@�@�r�@�hs@�C�@�9@���@�%@�@��@�7@��m@�C�@ޟ�@�n�@�5?@�{@ݲ-@ܣ�@�j@��@�C�@١�@�Q�@�|�@���@Ұ!@Ь@θR@��/@˝�@�o@ʏ\@�ff@�5?@�@ə�@Ɂ@�p�@�/@�Ĝ@�bN@�  @Ə\@��;@�+@�p�@� �@���@�
=@��+@�-@���@���@���@��@��@�O�@��@���@�^5@��@���@��y@���@�$�@���@��h@�`B@�V@���@��@��\@�-@�-@�J@��#@��@�G�@�%@��9@��@�bN@�1'@��@��w@��@��H@�ȴ@���@�V@�p�@��@��9@�z�@�I�@���@�K�@�;d@�+@���@���@���@�7L@�&�@��@��u@��;@��@��@�o@��R@�V@�p�@�X@�x�@��@�p�@��`@��@�n�@���@��`@���@�bN@��@��@��w@��@��@���@���@��P@�S�@���@�
=@���@��y@�@�o@�"�@�;d@�S�@�;d@��y@�ȴ@�ȴ@���@�ȴ@���@��!@��\@�n�@�V@�=q@�{@��#@���@�hs@�7L@�V@���@�Ĝ@��j@��@��@�  @�|�@�S�@�C�@�
=@��@���@�E�@�@�G�@�?}@���@��m@��P@��@��P@�|�@�+@���@��@�`B@�G�@��@�%@��/@���@��9@���@�bN@�I�@� �@�1@�  @��@��@�K�@���@�^5@�=q@�$�@�J@���@���@�p�@�&�@��D@�(�@�b@�  @��@���@��@��@�+@��y@���@�V@�$�@���@�x�@�%@���@�r�@�9XG�O�@�g8@��@|bN@t�@m/@i&�@]O�@W�P@R��@G��@=L�@;\)@7��@.�@)!�@#��@��@&�@8�@�@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�LBĜBƨBǮB��B�B�fB�B��B\B�B'�B(�B(�B)�B+B+B,B,B-B-B.B/B0!B1'B33B8RB;dB<jB>wBA�BI�BS�B_;B^5B_;Be`BiyBm�Bu�Bx�B�uB��B��B��B��B��B��B��B��B��B��B�{B�JB�7B�1B�+B�%B�B�B�Bw�BiyBaHBS�B;dB1'B,B%�B�B\BPBDB	7B+B%BB��B�B�B�NB�B��B��B�!B��B��B�VBz�BcTB\)BH�B5?B�BPB
�)B
��B
ÖB
�B
��B
��B
��B
��B
��B
��B
��B
�DB
s�B
gmB
VB
@�B
 �B
DB	��B	�B	�)B	��B	ŢB	�qB	�LB	�9B	��B	�uB	t�B	jB	ffB	_;B	P�B	?}B	6FB	0!B	-B	'�B	�B	hB	DB	B��B��B�B�B�sB�fB�TB�BB�B��B��B��B��BɺBǮBƨBĜBÖBB��B��B�}B�qB�jB�jB�dB�^B�XB�RB�LB�FB�FB��BŢBŢBÖBŢBȴB��B��B��B��B��B��B�ZB�B��B��B��B��B��B��B��B��B�B�B�B�B�ZB�HB�#B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBȴBĜB��B�XB�9B�'B�B�B�B�B�'B�3B�9B�9B�?B�?B�?B�XB�wB�}B�wB�}B�}B�}B��B��BŢB��B��B�#B�)B�5B�;B�;B�HB�HB�NB�HB�NB�TB�ZB�`B�sB�B�B��B��B	  B	B	B	%B	%B	+B	1B		7B	1B	1B	DB	bB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	(�B	,B	,B	-B	.B	49B	8RB	;dB	=qB	?}B	?}B	@�B	@�B	B�B	E�B	F�B	E�B	F�B	J�B	P�B	R�B	S�B	T�B	T�B	W
B	YB	YB	YB	ZB	ZB	]/B	^5B	^5B	^5B	`BB	aHB	aHB	`BB	_;B	_;B	^5B	]/B	]/B	^5B	_;B	_;B	`BB	bNB	ffB	hsB	l�B	m�B	n�B	o�B	p�B	q�B	r�B	r�B	r�B	r�B	r�B	t�B	x�B	y�B	{�B	�DB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�9B	�?B	�?B	�FB	�RB	�jB	��B	��B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�/B	�;B	�BB	�BB	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%B
�B
!HB
(�B
1�B
5?B
=�B
B�B
F�B
O�B
W�B
X�B
\CB
c B
g�B
l�B
q�B
u�B
y>B
}B
�4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�dB�cB�fB�aB�gB�gB�fB�gB�eB�eB�aB�gB�eB�fB�fB�fB�fB�fB�eB�jB�iB�jB�oB�{B�vB�sB�B�|B�B�B��B��B��B��B��B��B��B��B��B�B�`B�fB�nBɞB��B�)B�nB��B	B^B!�B"�B"�B#�B$�B$�B%�B%�B&�B&�B'�B(�B)�B*�B,�B2	B5%B6$B86B;GBCvBM�BX�BW�BX�B_Bc1BgHBo~Br�B�1B�YB��B��B��B�uB�|B�{B�yB�nB�]B�6B�B��B��B��B�B~�B}�B{�Bq�Bc6B[BM�B5$B*�B%�B�BTB	BBB�B �B��B��B��B�yB�LB�B��BȡB�IB��B��B�dB�"Bt�B]#BU�BB�B/B~B'B
��B
ɳB
�jB
��B
��B
��B
��B
��B
��B
�B
�gB
�B
m�B
aKB
O�B
:^B
�B
$B	��B	�gB	�B	��B	��B	�XB	�1B	�!B	��B	�[B	n�B	dgB	`NB	Y'B	J�B	9kB	03B	*B	&�B	!�B	�B	UB	2B�B��B��B�B�qB�dB�VB�GB�3B�B��B��BŹBĶBìB��B��B��B��B��B�~B�yB�pB�eB�bB�`B�ZB�RB�NB�GB�BB�;B�;B�xB��B��B��B��B¨BķB��B��B��B��B��B�OB�B��B��B��B��B��B��B��B�B�B�B�B�wB�NB�9B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BžBñB®B��B�yB�QB�2B�"B�B�B�B�B� B�,B�1B�1B�6B�:B�9B�OB�oB�uB�tB�wB�wB�uB�B�yB��B��B��B�B�"B�,B�4B�4B�BB�AB�GB�BB�FB�KB�UB�YB�lB�B��B��B��B��B�B�B	 B	 B	"B	)B	.B	)B	*B	9B	
\B	\B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	%�B	'B	(	B	.0B	2FB	5YB	7gB	9pB	9oB	:wB	:wB	<�B	?�B	@�B	?�B	@�B	D�B	J�B	L�B	M�B	N�B	N�B	QB	SB	SB	SB	TB	TB	W$B	X&B	X*B	X*B	Z6B	[:B	[;B	Z8B	Y-B	Y/B	X(B	W#B	W#B	X'B	Y.B	Y/B	Z6B	\BB	`[B	beB	f}B	g�B	h�B	i�B	j�B	k�B	l�B	l�B	l�B	l�B	l�B	n�B	r�B	s�B	u�B	�6B	�_B	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�+B	�-B	�-B	�6B	�CB	�YB	�pB	�}B	��B	��B	įB	ŶB	ƼB	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�*B	�2B	�2B	�*B	�0B	�4B	�8B	�4B	�8B	�5B	�BB	�AB	�JB	�HB	�IB	�KB	�GB	�GB	�HB	�EB	�IB	�IB	�HB	�HB	�HB	�FB	�JB	�UB	�lB	�xB	�{B	�yB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��G�O�B
 B
�B
3B
"�B
+|B
/'B
7xB
<�B
@�B
I�B
Q{B
R�B
V/B
]B
a�B
f�B
k�B
o�B
s,B
v�B
z!111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.006(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940252019060409402520190604094025  AO  ARCAADJP                                                                    20181121041156    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041156  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041156  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094025  IP                  G�O�G�O�G�O�                