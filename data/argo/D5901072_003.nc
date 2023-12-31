CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:40Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143511  20190522121828  1728_5048_003                   2C  D   APEX                            2142                            040306                          846 @�4| ���1   @�4|��@3*=p��
�c�t�j~�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�ffB���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C�fC  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD�fD  D� D  D� D  D� DfD�fD  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4y�D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D<��D=y�D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dyy�D�  D�C3D�� D��fD��3D�)�D���D���D��D�C3D�p DǖfD��fD�&fD�l�D� D�ɚD�C3D�\�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�ffB���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C�fC  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD�fD  D� D  D� D  D� DfD�fD  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4y�D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D<��D=y�D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dyy�D�  D�C3D�� D��fD��3D�)�D���D���D��D�C3D�p DǖfD��fD�&fD�l�D� D�ɚD�C3D�\�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A֛�A�ZA�-A��;A�(�A��AԍPA�bA�$�AҰ!Aҡ�Aҟ�Aҝ�Aқ�Aҕ�A�bNA�/A� �A�
=A���A�ĜAѰ!Aѧ�Aї�AуA�\)A�A��A��mA��TA��A�ĜAС�AГuAЛ�A;wA�+A�9XA��#A��;A���A���A�=qA�bNA�VA���A�\)A�oA�bNA�bNA���A���A�;dA�n�A���A��A�5?A���A�\)A���A���A�dZA�9XA��A���A�t�A�-A��#A��A�A��#A���A�G�A�JA��`A��wA�VA���A�&�A���A�bNA�  A�x�A�JA�z�A���A��;A���A���A��A���A��RA�(�A��A�I�A�&�A�M�A�S�A��A���A�5?A�{A�ffA��FA��yA��7A�A��A�ȴA�1'A��^A�I�A���A�+A�A�A��yA�=qA�jA�XA?}A{�#Axv�Awl�Avv�At�ArȴApA�An  Ag�;Ag�Afv�Ab��A^�`A[�wAX��AV�AShsAP�AN�AJ~�AJ�AD�RAB��ABVA@n�A>�uA<$�A9�A7p�A5\)A4r�A3�
A2�!A1S�A-��A,��A,ffA,�A+�A+S�A(v�A$�+A#dZA"E�A!&�A�Al�Az�AdZA��A;dA�`A~�Ap�AbNA7LAVAp�A�yA/A�+A1'A��A�A
��A
$�A	�
A	S�A�mA��Al�A��A��A�A��A��AJAO�A V@���@���@��@�J@��^@���@�?}@��@��D@�(�@�K�@���@���@�V@�|�@�5?@��T@�`B@�Z@�ȴ@�@�hs@���@�1'@�
=@�-@�?}@���@�j@�w@�
=@���@�+@��@�7@�/@�j@�S�@�"�@��@��y@�C�@�w@��m@���@�  @�1@�b@�  @��@���@�7@��/@�(�@�K�@�n�@�hs@��@ۍP@�+@��@�J@�7L@�\)@և+@�G�@�O�@�O�@�/@�%@Ԭ@�1@�t�@�C�@҇+@Ѳ-@�G�@�z�@�b@�  @��@Ͼw@�|�@�;d@���@�M�@�X@̣�@�z�@̣�@̃@�9X@���@͙�@�V@˕�@ɩ�@ɲ-@�-@�l�@�`B@ёh@�G�@��`@У�@�I�@�(�@���@���@ϝ�@���@��/@ʧ�@ɡ�@�/@ȓu@ƸR@���@�\)@�J@�x�@��@���@���@��D@�ƨ@��@���@��@�\)@�33@��H@���@��\@��+@��+@�~�@�=q@��@��P@�ȴ@��!@�n�@�5?@�{@�G�@�Ĝ@�9X@�  @��;@��
@�dZ@�v�@�J@�?}@�j@�@���@�x�@�`B@��@�r�@�  @��w@�|�@�o@��!@�~�@�=q@��7@�7L@�%@�z�@�b@��
@���@��@�l�@�S�@�;d@�@���@���@�=q@�@��@�I�@�  @��;@���@�K�@���@�^5@�n�@��\@�n�@��@�x�@�`B@�&�@��@�r�@�(�@���@�"�@��\@�{@���@��h@�x�@�?}@�%@��9@�j@��m@���@�33@��H@�ȴ@���@�n�@�-@�@��@��@��@��T@���@�@��^@���@�x�@��@��/@��u@�bN@�Z@�Q�@�I�@�9X@� �@�  @��w@�\)@�C�@�C�@�;d@�+@�@��y@���@�n�@�M�@�-@�{@��@���@���@�x�@�G�@��@��@���@���@��9@��u@�j@�Z@�I�@�9X@�(�@�b@���@��
@��w@��@���@�t�@�\)@�K�@�33@�o@��H@���@��!@���@�v�@�=q@�J@��@���@���@�z�@�;d@�|�@z��@q�@g\)@a��@Y�^@P�`@Fȴ@@�u@9�^@1�#@.��@)&�@#ƨ@��@G�@@G�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A֛�A�ZA�-A��;A�(�A��AԍPA�bA�$�AҰ!Aҡ�Aҟ�Aҝ�Aқ�Aҕ�A�bNA�/A� �A�
=A���A�ĜAѰ!Aѧ�Aї�AуA�\)A�A��A��mA��TA��A�ĜAС�AГuAЛ�A;wA�+A�9XA��#A��;A���A���A�=qA�bNA�VA���A�\)A�oA�bNA�bNA���A���A�;dA�n�A���A��A�5?A���A�\)A���A���A�dZA�9XA��A���A�t�A�-A��#A��A�A��#A���A�G�A�JA��`A��wA�VA���A�&�A���A�bNA�  A�x�A�JA�z�A���A��;A���A���A��A���A��RA�(�A��A�I�A�&�A�M�A�S�A��A���A�5?A�{A�ffA��FA��yA��7A�A��A�ȴA�1'A��^A�I�A���A�+A�A�A��yA�=qA�jA�XA?}A{�#Axv�Awl�Avv�At�ArȴApA�An  Ag�;Ag�Afv�Ab��A^�`A[�wAX��AV�AShsAP�AN�AJ~�AJ�AD�RAB��ABVA@n�A>�uA<$�A9�A7p�A5\)A4r�A3�
A2�!A1S�A-��A,��A,ffA,�A+�A+S�A(v�A$�+A#dZA"E�A!&�A�Al�Az�AdZA��A;dA�`A~�Ap�AbNA7LAVAp�A�yA/A�+A1'A��A�A
��A
$�A	�
A	S�A�mA��Al�A��A��A�A��A��AJAO�A V@���@���@��@�J@��^@���@�?}@��@��D@�(�@�K�@���@���@�V@�|�@�5?@��T@�`B@�Z@�ȴ@�@�hs@���@�1'@�
=@�-@�?}@���@�j@�w@�
=@���@�+@��@�7@�/@�j@�S�@�"�@��@��y@�C�@�w@��m@���@�  @�1@�b@�  @��@���@�7@��/@�(�@�K�@�n�@�hs@��@ۍP@�+@��@�J@�7L@�\)@և+@�G�@�O�@�O�@�/@�%@Ԭ@�1@�t�@�C�@҇+@Ѳ-@�G�@�z�@�b@�  @��@Ͼw@�|�@�;d@���@�M�@�X@̣�@�z�@̣�@̃@�9X@���@͙�@�V@˕�@ɩ�@ɲ-@�-@�l�@�`B@ёh@�G�@��`@У�@�I�@�(�@���@���@ϝ�@���@��/@ʧ�@ɡ�@�/@ȓu@ƸR@���@�\)@�J@�x�@��@���@���@��D@�ƨ@��@���@��@�\)@�33@��H@���@��\@��+@��+@�~�@�=q@��@��P@�ȴ@��!@�n�@�5?@�{@�G�@�Ĝ@�9X@�  @��;@��
@�dZ@�v�@�J@�?}@�j@�@���@�x�@�`B@��@�r�@�  @��w@�|�@�o@��!@�~�@�=q@��7@�7L@�%@�z�@�b@��
@���@��@�l�@�S�@�;d@�@���@���@�=q@�@��@�I�@�  @��;@���@�K�@���@�^5@�n�@��\@�n�@��@�x�@�`B@�&�@��@�r�@�(�@���@�"�@��\@�{@���@��h@�x�@�?}@�%@��9@�j@��m@���@�33@��H@�ȴ@���@�n�@�-@�@��@��@��@��T@���@�@��^@���@�x�@��@��/@��u@�bN@�Z@�Q�@�I�@�9X@� �@�  @��w@�\)@�C�@�C�@�;d@�+@�@��y@���@�n�@�M�@�-@�{@��@���@���@�x�@�G�@��@��@���@���@��9@��u@�j@�Z@�I�@�9X@�(�@�b@���@��
@��w@��@���@�t�@�\)@�K�@�33@�o@��H@���@��!@���@�v�@�=q@�J@��@���@���@�z�@�;d@�|�@z��@q�@g\)@a��@Y�^@P�`@Fȴ@@�u@9�^@1�#@.��@)&�@#ƨ@��@G�@@G�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
�
B
�B
�B
�
B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�B
�B
�#B
�/B
�fB
�yB
�B
�B
�B
��B
��BBB
�BB
�wB
ĜB
�B\B8RBn�B�+B�+B�7B��B�uB�B�jB��B�BD�BZBt�B�B�7B�uB��B�B�XB��BBŢB��B��B�
B�)B�NB�B��B��B��B��B+BPBuB�B�B$�B%�B"�B&�B.B49B-B-B1'B49B2-B,B�BB��B�B��B�}B�LB�B��B�VBcTBM�B<jB0!B�B�B��B��B�!B� B_;BuB
�}B
�B
��B
�B
ffB
W
B
H�B
;dB
%�B

=B
  B	�B	�sB	�)B	��B	�qB	��B	��B	��B	�bB	�B	o�B	dZB	I�B	E�B	C�B	%�B	/B	DB�B�B�HB�HB��B�wB�RB�!B��B��B��B��B��B��B��B��B��B��B��B��B�VB��B��B��B��B�=B�oB�{B��B�JB��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B�oB��B�{B��B��B��B��B��B�{B�{B��B�{B�oB�\B�hB�oB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�9B�?B�?B�LB�?B�?B�?B�9B�wB�jB�wB��BǮB��B�)B�NB�fB�mB�B�B�B�B�B�B�B�B�B��B��B�B��B��B��B��B��B	  B	B	
=B	JB	VB	\B	hB	uB	{B	uB	uB	�B	�B	�B	!�B	!�B	!�B	"�B	!�B	!�B	"�B	#�B	$�B	%�B	&�B	)�B	-B	+B	5?B	<jB	H�B	6FB	5?B	6FB	9XB	8RB	ZB	|�B	�+B	�%B	�%B	�7B	�7B	�7B	�=B	�=B	�7B	|�B	w�B	t�B	v�B	w�B	l�B	bNB	[#B	YB	VB	T�B	\)B	VB	XB	aHB	ffB	k�B	l�B	o�B	q�B	s�B	w�B	�B	�B	�+B	�1B	�7B	�+B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�+B	�1B	�7B	�JB	�\B	�PB	�PB	�bB	�\B	�VB	�hB	�oB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�?B	�3B	�3B	�3B	�?B	�9B	�?B	�?B	�LB	�dB	�qB	��B	B	ŢB	ƨB	ɺB	��B	��B	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�/B	�/B	�5B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�TB	�HB	�ZB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�`B	�`B	�`B	�`B	�fB	�yB	�sB	�yB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
	7B
PB
�B
�B
&�B
-B
49B
:^B
C�B
I�B
O�B
VB
YB
_;B
bNB
gmB
m�B
p�B
u�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�BB
�B
�B
�B
�B
�
B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�
B
�B
�B
�)B
�;B
�fB
�yB
�B
�B
�B
��B
��BBJB
�B
B
ƨB
��BuB8RBp�B�DB�7B�=B��B��B�'B�}B�B�BF�B]/Bw�B�B�=B��B��B�!B�dB��BÖBǮB��B��B�B�5B�mB�B��B��B��BB1BVB�B�B �B&�B'�B$�B(�B0!B6FB/B.B1'B5?B49B1'B�B1B��B�B�BĜB�qB�-B�B��Bk�BR�BA�B5?B!�B�B1B��B�FB�%Bk�B�B
ŢB
�!B
��B
�bB
o�B
\)B
L�B
D�B
/B
PB
B	��B	�B	�TB	�B	��B	��B	��B	��B	��B	�+B	u�B	hsB	Q�B	L�B	I�B	/B	1'B	�B�B�B�fB�fB�BŢB�}B�FB��B�B��B�B�B��B��B��B��B��B��B��B�oB��B��B��B��B�hB�{B��B��B�bB��B��B��B��B��B��B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�{B�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�3B�?B�?B�FB�RB�FB�FB�FB�FB�wB�jB�wB��BƨB��B�)B�NB�fB�mB�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	B	
=B	JB	VB	\B	oB	{B	�B	�B	�B	�B	�B	�B	!�B	!�B	!�B	#�B	"�B	"�B	#�B	%�B	%�B	%�B	&�B	)�B	-B	)�B	49B	=qB	J�B	8RB	5?B	5?B	7LB	5?B	T�B	|�B	�1B	�%B	�+B	�7B	�7B	�7B	�=B	�DB	�JB	� B	y�B	u�B	w�B	z�B	o�B	dZB	]/B	ZB	W
B	W
B	_;B	XB	YB	aHB	ffB	k�B	l�B	o�B	r�B	s�B	w�B	�B	�B	�+B	�7B	�DB	�=B	�B	�B	�B	�B	�B	�%B	�+B	�+B	�+B	�+B	�+B	�7B	�DB	�PB	�bB	�\B	�bB	�oB	�bB	�VB	�hB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�FB	�3B	�3B	�3B	�FB	�?B	�FB	�?B	�LB	�dB	�wB	��B	B	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�/B	�/B	�;B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�BB	�TB	�HB	�ZB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�sB	�`B	�`B	�`B	�`B	�fB	�yB	�yB	�yB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
	7B
PB
�B
�B
&�B
-B
49B
:^B
C�B
I�B
O�B
VB
YB
_;B
bNB
gmB
m�B
p�B
u�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<D��<D��<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451532012011014515320120110145153  AO  ARGQ                                                                        20111130143511  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143511  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145153  IP                  G�O�G�O�G�O�                