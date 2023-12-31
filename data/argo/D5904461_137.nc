CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:58:27Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125827  20190408133246  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286                            2C  D   APEX                            6531                            072314                          846 @�ŧX�91   @�ŧ�$L@5
=p���b�7Kƨ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D��D�L�D�� D���D�fD�FfD�0 D��3D���D�C3D�y�D�� D�	�D�9�Dڐ D�ٚD���D�33D�l�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��G@�z�A=qA"=qAB=qAb=qA��A��A��A��A��A��A��A��B �\B�\B�\B�\B �\B(�\B0�\B8��B@�\BH�\BP�\BX�\B`�\Bh�\Bp�\Bx�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C #�C"#�C$#�C&#�C(#�C*#�C,#�C.#�C0#�C2#�C4#�C6#�C8#�C:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT#�CV#�CX#�CZ#�C\#�C^#�C`=qCb#�Cd#�Cf#�Ch#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv#�Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D \D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy��D�D�QHD��{D��HD�
�D�J�D�4{D�ǮD�HD�G�D�~D��{D�D�>Dڔ{D��D��D�7�D�qHD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�~�A�~�A�|�A�~�AԁAԅA�z�A�JA�jA�K�A�E�A�=qA�&�A��TA���Aҥ�A�z�A�G�A�  A��#A�{A�
=AѓuA�r�A�ffA�E�A�{A��#A�5?A�{A��A�dZAȲ-A�v�A�C�A�`BA�=qAǅA�-A��TA�ȴA��A�\)AĲ-A�A�A�oAþwA��HA��PA�~�A��yA��+A�I�A��A�l�A�ƨA�v�A�ZA�\)A��A��FA��/A�A�z�A���A�Q�A���A��-A��DA�G�A���A��A�VA�M�A���A���A�I�A���A��uA���A�JA�1A��HA���A��PA�^5A�E�A��DA��hA� �A��TA�ƨA���A�+A��^A�S�A�;dA��^A�M�A���A��`A��hA�oA��wA���A���A�=qA�`BA���A�A�A~E�A|�Ay��Ax~�Aw�FAvArAn��AlJAj�Ah��Ah�+AhA�Afr�AdI�Ab(�A^�/A]VA[%AX^5AV�/AU�AS��AR�AP~�AM��AKhsAH�\AG?}AFJAE�AC�wABQ�AA+A@-A?33A>  A<��A:��A9VA6��A5�A3�hA1�
A/�-A-�
A,�RA+��A)�hA)&�A'��A&bNA%�A$M�A"��A"{A!?}A��A7LAffA��Av�AA�A�AffA1'A�7AA�
A�^A�AI�AO�AbA��A�A��A�/A=qA��A��A��A^5A��At�A
��A
M�A
ffA	VA��A1At�A�!An�A�
A�DAJA1'A ��A ��A 5?A �@�o@��@��/@�1'@���@�M�@�E�@�=q@��@�&�@��@�t�@��@��#@�bN@��@�=q@�1'@�n�@�x�@���@��@���@��@��@�C�@���@旍@�h@�j@���@�z�@��@޸R@�=q@�hs@�bN@�
=@���@�V@؛�@ו�@�$�@�G�@���@�9X@�ƨ@�;d@�-@�p�@���@�I�@�?}@�l�@�ȴ@͙�@�O�@�V@���@��@��@���@��`@��/@��@���@�9X@�ȴ@��@��@�ff@ɩ�@�?}@�Ĝ@ț�@�r�@ȋD@�|�@�ȴ@�V@���@�hs@�A�@�@�p�@��@��@�^5@ÍP@�o@���@�1@�@���@�=q@�O�@���@�n�@�J@�@�j@��@�5?@��u@���@�o@�O�@��F@�dZ@��H@��@� �@�I�@��w@��m@�ff@��T@��T@�@���@��^@��^@�@���@��T@�J@��@�v�@���@���@�t�@�C�@�E�@�%@��h@�J@�C�@�M�@�A�@��
@�{@��@��@�C�@�Ĝ@�`B@�Z@��@�7L@�b@���@�  @�(�@��@�A�@�bN@��u@���@���@�bN@���@���@��F@��@�t�@���@�=q@��7@�9X@�33@�{@�-@��T@�5?@���@��@���@�@�v�@��@��h@��-@�E�@��\@���@��/@�Z@�A�@��m@�K�@�Q�@��F@���@�l�@��\@�~�@��@�
=@��@�^5@�ff@�M�@���@�G�@���@�%@��9@�1@��
@��w@�33@��+@��+@�ȴ@�+@�+@�o@���@��y@��H@��+@�V@���@��7@�`B@��@��@��@��j@���@�z�@�A�@� �@�  @��m@��P@�dZ@��@�t�@��@�K�@�+@�ȴ@�n�@�-@�p�@�O�@�&�@���@�z�@�9X@�b@��
@��w@���@���@�
=@��!@���@��+@�n�@�V@�=q@�5?@�-@�J@�@��@�O�@�7L@�V@��u@�(�@��
@��F@�K�@�O�@�Q�@~E�@sS�@k@b�\@[@T�@K��@D�@>$�@7�@1��@,��@(  @!�7@�@Q�@�h@Q�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�~�A�~�A�|�A�~�AԁAԅA�z�A�JA�jA�K�A�E�A�=qA�&�A��TA���Aҥ�A�z�A�G�A�  A��#A�{A�
=AѓuA�r�A�ffA�E�A�{A��#A�5?A�{A��A�dZAȲ-A�v�A�C�A�`BA�=qAǅA�-A��TA�ȴA��A�\)AĲ-A�A�A�oAþwA��HA��PA�~�A��yA��+A�I�A��A�l�A�ƨA�v�A�ZA�\)A��A��FA��/A�A�z�A���A�Q�A���A��-A��DA�G�A���A��A�VA�M�A���A���A�I�A���A��uA���A�JA�1A��HA���A��PA�^5A�E�A��DA��hA� �A��TA�ƨA���A�+A��^A�S�A�;dA��^A�M�A���A��`A��hA�oA��wA���A���A�=qA�`BA���A�A�A~E�A|�Ay��Ax~�Aw�FAvArAn��AlJAj�Ah��Ah�+AhA�Afr�AdI�Ab(�A^�/A]VA[%AX^5AV�/AU�AS��AR�AP~�AM��AKhsAH�\AG?}AFJAE�AC�wABQ�AA+A@-A?33A>  A<��A:��A9VA6��A5�A3�hA1�
A/�-A-�
A,�RA+��A)�hA)&�A'��A&bNA%�A$M�A"��A"{A!?}A��A7LAffA��Av�AA�A�AffA1'A�7AA�
A�^A�AI�AO�AbA��A�A��A�/A=qA��A��A��A^5A��At�A
��A
M�A
ffA	VA��A1At�A�!An�A�
A�DAJA1'A ��A ��A 5?A �@�o@��@��/@�1'@���@�M�@�E�@�=q@��@�&�@��@�t�@��@��#@�bN@��@�=q@�1'@�n�@�x�@���@��@���@��@��@�C�@���@旍@�h@�j@���@�z�@��@޸R@�=q@�hs@�bN@�
=@���@�V@؛�@ו�@�$�@�G�@���@�9X@�ƨ@�;d@�-@�p�@���@�I�@�?}@�l�@�ȴ@͙�@�O�@�V@���@��@��@���@��`@��/@��@���@�9X@�ȴ@��@��@�ff@ɩ�@�?}@�Ĝ@ț�@�r�@ȋD@�|�@�ȴ@�V@���@�hs@�A�@�@�p�@��@��@�^5@ÍP@�o@���@�1@�@���@�=q@�O�@���@�n�@�J@�@�j@��@�5?@��u@���@�o@�O�@��F@�dZ@��H@��@� �@�I�@��w@��m@�ff@��T@��T@�@���@��^@��^@�@���@��T@�J@��@�v�@���@���@�t�@�C�@�E�@�%@��h@�J@�C�@�M�@�A�@��
@�{@��@��@�C�@�Ĝ@�`B@�Z@��@�7L@�b@���@�  @�(�@��@�A�@�bN@��u@���@���@�bN@���@���@��F@��@�t�@���@�=q@��7@�9X@�33@�{@�-@��T@�5?@���@��@���@�@�v�@��@��h@��-@�E�@��\@���@��/@�Z@�A�@��m@�K�@�Q�@��F@���@�l�@��\@�~�@��@�
=@��@�^5@�ff@�M�@���@�G�@���@�%@��9@�1@��
@��w@�33@��+@��+@�ȴ@�+@�+@�o@���@��y@��H@��+@�V@���@��7@�`B@��@��@��@��j@���@�z�@�A�@� �@�  @��m@��P@�dZ@��@�t�@��@�K�@�+@�ȴ@�n�@�-@�p�@�O�@�&�@���@�z�@�9X@�b@��
@��w@���@���@�
=@��!@���@��+@�n�@�V@�=q@�5?@�-@�J@�@��@�O�@�7L@�V@��u@�(�@��
@��F@�K�@�O�@�Q�@~E�@sS�@k@b�\@[@T�@K��@D�@>$�@7�@1��@,��@(  @!�7@�@Q�@�h@Q�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
�B
�B
�?B
B
��B
�`B{B$�B/B.B,B.B7LBG�B^5B~�B{�BN�B)�BF�B�B�B�/BbB�B#�B$�B)�B:^BB�BC�BA�B;dB-B��B�B��B�)B�/B�BŢB�B��B{BI�B�B�^B�dB�^B��B��B��B�?BĜBɺB��B�BÖB�9B��B�{B�bB�=B�B�7B�oBE�B�B�;B�%Bt�Bq�Bn�B�+B�B~�Bz�BiyB^5BdZBJ�B%�B
�B
�RB
�=B
s�B
dZB
W
B
:^B
1'B
%�B
�B
JB	��B	�B	�B	ɺB	��B	�B	��B	��B	�JB	p�B	`BB	Q�B	G�B	C�B	G�B	I�B	A�B	8RB	2-B	+B	#�B	�B	DB	B��B�B�sB�;B��BɺB�}B�dB�RB�?B�'B�B�B��B��B��B��B��B�{B��B��B��B�JB�%B�DB��B��B��B�{B�PB�1B�+B�1B�B�B�B�+B�B}�Bx�B}�Bw�Br�Bs�Br�Bp�Bn�Bo�Br�Bt�Bp�Bk�BgmBgmBk�Bq�Bo�Bl�BgmBbNB`BB_;B_;B]/B[#B\)BjBhsBm�Bs�Bu�Bv�Bv�Bt�Bq�Bq�Bk�BgmBgmBffBffBiyBk�Bk�BiyBffBjBq�Bu�By�B}�B�B�B�B�B�+B�7B�7B�JB�PB�PB�PB�VB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�FB�wBŢBȴBȴBǮBȴBɺB��B��B��B�
B�TB�NB�TB�TB�B�B�B�B��B��B��B��B	B	+B	+B	%B	JB	VB	bB	oB	�B	�B	�B	�B	!�B	!�B	 �B	#�B	!�B	&�B	&�B	&�B	#�B	�B	,B	9XB	D�B	E�B	A�B	=qB	<jB	<jB	;dB	9XB	49B	0!B	1'B	6FB	G�B	K�B	E�B	B�B	?}B	<jB	8RB	6FB	5?B	6FB	2-B	49B	8RB	8RB	=qB	:^B	;dB	<jB	<jB	=qB	?}B	E�B	J�B	M�B	O�B	R�B	R�B	YB	W
B	T�B	N�B	M�B	M�B	J�B	N�B	R�B	ZB	W
B	P�B	R�B	dZB	m�B	l�B	k�B	cTB	XB	VB	T�B	`BB	v�B	u�B	x�B	� B	�+B	�+B	�1B	�JB	�\B	�hB	�uB	��B	��B	��B	��B	��B	�uB	�{B	�hB	�PB	�=B	�7B	�=B	�DB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�!B	�3B	�?B	�FB	�LB	�LB	�RB	�XB	�XB	�XB	�qB	��B	��B	��B	��B	B	ÖB	ĜB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�TB	�ZB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
B
PB
uB
�B
#�B
(�B
33B
:^B
@�B
C�B
I�B
O�B
VB
[#B
`BB
dZB
iyB
k�B
p�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
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
�	B
�3B
B
��B
�VBqB$�B/B.	B+�B.
B7?BG�B^,B~�B{�BN�B)�BF�B�B��B�#BUB�B#�B$�B)�B:TBB�BC�BA{B;XB-B��B�B��B�B�"B��BŖB�B��BrBI�B��B�VB�WB�RB˼BʵB�zB�0BđBɯB��B�BÇB�/B��B�nB�XB�2B�B�*B�dBE�B�pB�0B�Bt�Bq�Bn�B�B�B~�Bz�BikB^)BdLBJ�B%�B
�B
�CB
�1B
s�B
dNB
V�B
:SB
1B
%�B
~B
=B	��B	�pB	��B	ɩB	�uB	�B	��B	��B	�<B	p�B	`5B	Q�B	G�B	C�B	G�B	I�B	AzB	8EB	2B	*�B	#�B	�B	5B	B��B�B�dB�.B��BɪB�nB�TB�BB�/B�B�B��B��B��B��B��B��B�jB��B��B�oB�;B�B�4B�{B�B�vB�kB�@B�!B�B�B�B�B�B�B�
B}�Bx�B}�Bw�Br�Bs�Br�Bp�Bn�Bo�Br�Bt�Bp�BkuBg[Bg\BkuBq�Bo�BlyBg\Bb;B`1B_,B_*B]B[B\BjoBhcBm�Bs�Bu�Bv�Bv�Bt�Bq�Bq�BkuBg]Bg^BfVBfWBihBktBksBiiBfVBjnBq�Bu�By�B}�B��B�B�B�B�B�&B�&B�7B�>B�=B�?B�EB�^B�cB�kB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�4B�dBőBȢBȢBǜBȡBɧB;B��B��B��B�BB�:B�CB�BB�B�B�B�B��B��B��B��B	B	B	B	B	7B	DB	PB	]B	nB	{B	�B	�B	!�B	!�B	 �B	#�B	!�B	&�B	&�B	&�B	#�B	�B	+�B	9GB	D�B	E�B	AxB	=]B	<XB	<ZB	;QB	9GB	4(B	0B	1B	63B	G�B	K�B	E�B	BB	?kB	<YB	8@B	63B	5+B	65B	2B	4$B	8BB	8?B	=_B	:MB	;RB	<XB	<XB	=`B	?lB	E�B	J�B	M�B	O�B	R�B	R�B	YB	V�B	T�B	N�B	M�B	M�B	J�B	N�B	R�B	Z	B	V�B	P�B	R�B	dHB	m�B	lvB	ksB	cBB	W�B	U�B	T�B	`0B	v�B	u�B	x�B	�B	�B	�B	� B	�6B	�JB	�XB	�bB	�nB	�oB	�vB	�tB	�pB	�dB	�lB	�WB	�?B	�+B	�%B	�+B	�3B	�YB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�-B	�4B	�;B	�9B	�@B	�EB	�EB	�EB	�^B	�nB	�tB	�wB	�wB	�|B	ÄB	ĈB	ƗB	ƘB	ǜB	ʫB	˴B	̺B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�!B	�*B	�0B	�3B	�@B	�IB	�SB	�TB	�[B	�aB	�gB	�dB	�fB	�dB	�rB	�vB	�B	�~B	�B	�B	�B	�B	�B	�B	��B
B
=B
cB
�B
#�B
(�B
3#B
:MB
@pB
C�B
I�B
O�B
U�B
[B
`0B
dHB
ihB
ksB
p�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.14 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904081332462019040813324620190408133246  AO  ARCAADJP                                                                    20181121125827    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125827  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125827  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190408133246  IP                  G�O�G�O�G�O�                