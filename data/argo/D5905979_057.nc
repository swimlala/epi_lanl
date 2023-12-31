CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:07Z creation      
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
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170907  20220204114416  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               9A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @إ\���1   @إ]/hZ2@6�n��O��c���`A�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    9A   B   B   @���@���A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B/��B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� DmfDm�fDn  Dny�Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy��D�qD�N�D��D��\D�HD�PRD��fD���D� RD�T{D��\D�� D��D�_
Dڨ D��qD�D�]�D��D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@��
@�
=A�A?�A_�A}�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'z�B/z�B7�HB?�HBG�HBO�HBXG�B_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC<�C=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{޸C}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D�zD�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~DmzDm�zDm�Dnw�Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�DtdzDy�
D�{D�M�D��D��fD�RD�O\D��pD���D�\D�S�D��fD��
D��D�^Dڧ
D��{D�)D�\�D��D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A��A��#A��#A��A��/A��#A��#A��/A��;A��mA��A��A��A��A��A���A��^A���A��7A�jA�=qA�oA���A���A���A���A���A�  A�A���A���A���A���A�A���A��A��A��mA��;A���A���A���A�ȴA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA���A���A���A���A�ȴA�ĜA��FA�-A�+A���A�jA�^5A��`A�=qA�ZA�&�A�&�A��;A�&�A��;A��A���A��RA��^A��jA�n�A�A��+A�jA���A�jA���A���A���A�A�^5A�A�dZA�/A��A�
=A���A��uA���A���A��A���A���A��A��!A�bNA�ZA�ƨA�33A�O�A��A�A�A��A���A�$�Ap�A|��AwXAuC�Asx�Ap�RAn�RAm�FAl��Al �Aj��Ai�AdQ�AaA_�A_/A^��A[��AY�7AX�/AX�AW�AV�AT��AS�ASVAQ7LAPANv�AM�ALjAKƨAK��AJ9XAH��AG�TAF�`AD�!ADAC�hABA�AA�A@jA>��A=dZA;��A:r�A9��A8�A7S�A5�;A3��A2ȴA2~�A25?A1�-A17LA0��A/�PA-�
A-XA+�wA)��A'�mA&��A%�hA$�!A#�;A#&�A"�A"VA!A!K�A!/A �A �9A Q�A&�A33AAE�A�AJA;dAoAVA�RA1'A��A�An�A�mA?}Av�A�^A��A�#AG�A��Al�A
�`A
A	��A	�hA	/Ap�A�uAbA��Az�A��Al�A ��@���@�5?@�@�7L@�Ĝ@�b@�"�@�^5@��@�@���@�  @��
@�|�@�@�^@���@�F@�n�@�A�@���@�%@㝲@��y@ᙚ@���@ް!@��
@�t�@��H@�X@�Z@�33@�Z@��H@�$�@ёh@�?}@�Q�@Χ�@���@�7L@� �@˶F@�J@ȴ9@�(�@�1@�o@�@��@�1@§�@��T@��@��@��/@��9@�9X@��@�
=@�@�bN@�;d@��@��7@��@�Q�@�;d@�5?@��/@���@�\)@�33@��@�o@�o@�
=@��@�V@�O�@�l�@��\@��@�/@��j@��F@��P@�t�@���@���@�x�@��@�r�@�(�@��;@���@�dZ@��H@�@���@��7@���@���@�1'@�  @��w@�l�@�"�@���@��\@��@�@��@�`B@�G�@�%@��/@��@��@�Z@�1'@�b@�b@�b@���@��@�S�@�
=@��\@�$�@��#@���@��-@���@���@��h@�O�@��j@�Z@�1'@��m@�l�@�K�@�+@��@�@��@���@��+@�ff@�@��T@���@��-@��h@��7@�p�@��@��9@��@��D@��@�ƨ@��F@��@���@���@���@��P@�t�@�\)@�;d@�;d@�o@��!@���@��+@�ff@�J@��#@�p�@�&�@�Ĝ@�b@�\)@�;d@��y@��@�ȴ@���@���@���@��R@���@��@���@��@�`B@�7L@��@��@�V@��@��D@� �@��F@�
=@��\@�=q@�@�&�@�I�@��w@��
@�|�@��@���@�@��h@�x�@�O�@��@���@��@�9X@��@�b@��m@���@���@�;d@���@���@��!@���@��!@�V@��@��@�p�@�7L@�G�@�X@�X@�Ĝ@�bN@��;@��w@��F@��F@���@���@��@��@���@��\@�M�@��@��@��T@��#@�@��'@|Z@t�P@j�@_P�@U�@O�@H��@B�@=0�@5%F@.#:@)8�@$�@ ��@@@@��@�g@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���A��A��#A��#A��A��/A��#A��#A��/A��;A��mA��A��A��A��A��A���A��^A���A��7A�jA�=qA�oA���A���A���A���A���A�  A�A���A���A���A���A�A���A��A��A��mA��;A���A���A���A�ȴA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA���A���A���A���A�ȴA�ĜA��FA�-A�+A���A�jA�^5A��`A�=qA�ZA�&�A�&�A��;A�&�A��;A��A���A��RA��^A��jA�n�A�A��+A�jA���A�jA���A���A���A�A�^5A�A�dZA�/A��A�
=A���A��uA���A���A��A���A���A��A��!A�bNA�ZA�ƨA�33A�O�A��A�A�A��A���A�$�Ap�A|��AwXAuC�Asx�Ap�RAn�RAm�FAl��Al �Aj��Ai�AdQ�AaA_�A_/A^��A[��AY�7AX�/AX�AW�AV�AT��AS�ASVAQ7LAPANv�AM�ALjAKƨAK��AJ9XAH��AG�TAF�`AD�!ADAC�hABA�AA�A@jA>��A=dZA;��A:r�A9��A8�A7S�A5�;A3��A2ȴA2~�A25?A1�-A17LA0��A/�PA-�
A-XA+�wA)��A'�mA&��A%�hA$�!A#�;A#&�A"�A"VA!A!K�A!/A �A �9A Q�A&�A33AAE�A�AJA;dAoAVA�RA1'A��A�An�A�mA?}Av�A�^A��A�#AG�A��Al�A
�`A
A	��A	�hA	/Ap�A�uAbA��Az�A��Al�A ��@���@�5?@�@�7L@�Ĝ@�b@�"�@�^5@��@�@���@�  @��
@�|�@�@�^@���@�F@�n�@�A�@���@�%@㝲@��y@ᙚ@���@ް!@��
@�t�@��H@�X@�Z@�33@�Z@��H@�$�@ёh@�?}@�Q�@Χ�@���@�7L@� �@˶F@�J@ȴ9@�(�@�1@�o@�@��@�1@§�@��T@��@��@��/@��9@�9X@��@�
=@�@�bN@�;d@��@��7@��@�Q�@�;d@�5?@��/@���@�\)@�33@��@�o@�o@�
=@��@�V@�O�@�l�@��\@��@�/@��j@��F@��P@�t�@���@���@�x�@��@�r�@�(�@��;@���@�dZ@��H@�@���@��7@���@���@�1'@�  @��w@�l�@�"�@���@��\@��@�@��@�`B@�G�@�%@��/@��@��@�Z@�1'@�b@�b@�b@���@��@�S�@�
=@��\@�$�@��#@���@��-@���@���@��h@�O�@��j@�Z@�1'@��m@�l�@�K�@�+@��@�@��@���@��+@�ff@�@��T@���@��-@��h@��7@�p�@��@��9@��@��D@��@�ƨ@��F@��@���@���@���@��P@�t�@�\)@�;d@�;d@�o@��!@���@��+@�ff@�J@��#@�p�@�&�@�Ĝ@�b@�\)@�;d@��y@��@�ȴ@���@���@���@��R@���@��@���@��@�`B@�7L@��@��@�V@��@��D@� �@��F@�
=@��\@�=q@�@�&�@�I�@��w@��
@�|�@��@���@�@��h@�x�@�O�@��@���@��@�9X@��@�b@��m@���@���@�;d@���@���@��!@���@��!@�V@��@��@�p�@�7L@�G�@�X@�X@�Ĝ@�bN@��;@��w@��F@��F@���@���@��@��@���@��\@�M�@��@��@��T@��#G�O�@��'@|Z@t�P@j�@_P�@U�@O�@H��@B�@=0�@5%F@.#:@)8�@$�@ ��@@@@��@�g@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB"�B#�B#�B"�B"�B"�B"�B"�B"�B!�B!�B#�B%�B'�B'�B)�B5?B[#B`BBgmBn�Bt�B{�B�B�B�B�B�B�B�B�B�%B�%B�+B�+B�=B�VB�bB�hB�oB�uB�uB�{B�{B��B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B�PB�\B�DB�+B�DB�PB�B~�Bt�Bp�Bm�Bn�BgmBffBcTBbNB\)BVBS�BQ�BO�BI�BC�B8RB1'B"�BuB+BBB  B��B�B�HB�
B�B�uBp�B]/BQ�B;dBB
�TB
�FB
��B
��B
��B
�bB
�B
t�B
o�B
jB
R�B
=qB
oB	��B	�B	�NB	�
B	��B	ȴB	B	�^B	�!B	��B	�B	x�B	q�B	m�B	aHB	P�B	J�B	H�B	E�B	>wB	5?B	0!B	,B	#�B	�B	�B	oB	PB		7B	1B	B��B��B��B�yB�NB�HB�)B�B��BǮB�jB�-B��B��B��B��B��B�oB�VB�JB�DB�7B�+B�B�B{�Bv�Bs�Bl�BhsBdZBbNB_;B]/B[#BZBYBXBVBVBVBS�BR�BP�BL�BG�BH�BE�BE�BD�BC�BC�BC�BB�BA�B@�B=qB<jB;dB:^B9XB8RB7LB6FB5?B5?B33B33B2-B1'B0!B0!B.B,B,B-B(�B'�B'�B$�B#�B$�B$�B$�B$�B$�B"�B#�B$�B#�B"�B"�B"�B"�B"�B!�B"�B �B"�B!�B(�B#�B#�B$�B#�B(�B(�B(�B)�B+B+B,B2-B2-B33B49B49B6FB:^B:^B=qB?}B@�BE�BJ�BL�BM�BR�BZB]/BcTBn�Bs�Bv�Bx�Bz�B{�B}�B�B�B�1B�{B��B��B��B��B��B��B��B�B�-B�9B�?B�?B�FB�FB�FB�FB�LB�jBŢBȴB��B��B��B�B�B�B�;B�TB�B�B��B��B��B��B��B��B��B	  B	B	+B	JB	bB	hB	oB	{B	{B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	$�B	&�B	(�B	+B	,B	,B	,B	-B	1'B	6FB	9XB	@�B	F�B	K�B	L�B	M�B	M�B	M�B	M�B	P�B	YB	]/B	_;B	cTB	iyB	jB	l�B	m�B	n�B	p�B	t�B	t�B	v�B	|�B	� B	�B	�B	�+B	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�?B	�LB	�^B	�^B	�jB	�jB	�jB	�jB	�jB	�jB	�jB	�qB	�}B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	�
B	�B	�
B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�B	�B	�#B	�B	�B	�#B	�#B	�)B	�;B	�HB	�HB	�NB	�ZB	�mB	�mB	�mB	�fB	�fB	�fB	�sB	�sB	�sB	�sB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�FB	�jB
�B
�B
=B
%zB
)_B
2�B
@�B
EB
L~B
S�B
X�B
]�B
a�B
e�B
k�B
o B
q�B
v`B
{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B-BR�BXB_EBfpBl�Bs�Bz�By�By�Bz�Bz�B{�B|�B|�B}�B}�BBB�B�,B�8B�>B�EB�KB�KB�PB�PB�VB�PB�PB�PB�VB�VB�VB�VB�\B�\B�\B�\B�VB�VB�bB�WB�&B�2B�BB�B�'Bx�Bv�Bl�Bh~BekBfrB_HB^AB[/BZ*BTBM�BK�BI�BG�BA�B;tB01B)B�BWB�B�B��B��B��B�B�-B��B�B�`Bh�BUBI�B3UB
��B
�KB
�@B
��B
��B
��B
�_B
yB
l�B
g�B
bB
J�B
5tB

uB	��B	�B	�XB	�B	��B	��B	��B	�kB	�/B	��B	{#B	p�B	i�B	e�B	Y\B	H�B	B�B	@�B	=�B	6�B	-VB	(8B	$ B	�B	�B	�B	
�B	jB	QB	 KB�:B�B��B��B�B�kB�fB�GB�.B��B��B��B�NB�B�B��B��B��B��B�zB�nB�hB�\BPB}DB{7BtBn�Bk�Bd�B`�B\�BZvBWcBUXBSLBRFBQ@BP9BN-BN-BN-BL!BKBIBD�B?�B@�B=�B=�B<�B;�B;�B;�B:�B9�B8�B5�B4�B3�B2�B1�B0B/yB.sB-lB-mB+aB+aB*[B)UB(OB(OB&BB$7B$7B%=B!%B B BBBBBBBBBBBBBBBBB�BB�BB�B!'BBBB	B!'B!'B!(B"-B#4B#4B$:B*^B*^B+dB,jB,jB.wB2�B2�B5�B7�B8�B=�BB�BD�BFBK"BRMBU_B[�Bf�Bk�Bn�BqBsBtBv"By4B|GB�_B��B��B��B��B��B��B�	B�B�:B�YB�eB�jB�jB�qB�qB�qB�qB�wB��B��B��B��B�	B�B�AB�GB�GB�dB�}B�B��B��B��B�B�B�	B�B�B�(B�.B�RB	qB	�B		�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	!B	#(B	$.B	$.B	$.B	%3B	)LB	.kB	1}B	8�B	>�B	C�B	D�B	E�B	E�B	E�B	E�B	IB	Q:B	URB	W^B	[vB	a�B	b�B	d�B	e�B	f�B	h�B	l�B	l�B	n�B	uB	x!B	z-B	|:B	KB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�2B	�9B	�KB	�QB	�]B	�jB	�|B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�!B	�'B	�3B	�4B	�9B	�?B	�?B	�?B	�?B	�?B	�:B	�:B	�@B	�:B	�:B	�@B	�@B	�FB	�WB	�dB	�dB	�jB	�vB	߉B	߉B	߉B	ނB	ނB	ނB	��B	��B	��B	��B	߉B	߉B	߉B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��G�O�B	�aB	�B	��B
�B
WB
�B
!yB
+B
8�B
=8B
D�B
K�B
P�B
U�B
Y�B
]�B
c�B
gB
i�B
nxB
s-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144162022020411441620220204114416  AO  ARCAADJP                                                                    20200619170907    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170907  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170907  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114416  IP                  G�O�G�O�G�O�                