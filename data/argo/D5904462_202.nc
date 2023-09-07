CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-07-14T00:01:08Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20170714000108  20190405100805  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�n
���1   @�n��f�@,��x����dgl�C��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C  C  C	�fC�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dyl�D�3D�6fD�i�D��3D�fD�L�D�i�D��3D���D�C3D���D�ɚD� D�@ Dڌ�D�ɚD�3D�)�D�i�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@љ�A��A(��AH��Ah��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B
33B33B��B"33B*33B233B:33BB33BJ33BR33BZ33Bb33Bj33Br33Bz33B��B��B��B��B��B��B��B��B��B��B��B��B�L�B��B��gB��B��B��B��B��B��B�L�B��gB��gB��B��B��B��B��B��B��B��C ��C�gC�gC��C��C
s3Cs3C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.�gC0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�FfC�S3C�S3C�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfD #3D �3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D	#3D	�3D
#3D
�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D�D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D #3D �3D!#3D!�3D"#3D"�3D##3D#�3D$#3D$�3D%#3D%�3D&#3D&�3D'#3D'�3D(#3D(�3D)#3D)�3D*#3D*�3D+#3D+�3D,#3D,�3D-#3D-�3D.#3D.�3D/#3D/�3D0#3D0�3D1#3D1�3D2#3D2�3D3#3D3�3D4#3D4�3D5#3D5�3D6#3D6�3D7#3D7�3D8#3D8�3D9#3D9�3D:#3D:�3D;#3D;�3D<#3D<�3D=#3D=�3D>#3D>�3D?#3D?�3D@#3D@�3DA#3DA�3DB#3DB�3DC#3DC�3DD#3DD�3DE#3DE�3DF#3DF�3DG#3DG�3DH#3DH�3DI#3DI�3DJ#3DJ�3DK)�DK�3DL#3DL�3DM#3DM�3DN#3DN�3DO#3DO�3DP#3DP�3DQ#3DQ�3DR#3DR�3DS#3DS�3DT#3DT�3DU#3DU�3DV#3DV�3DW#3DW�3DX#3DX�3DY#3DY�3DZ#3DZ�3D[#3D[�3D\#3D\�3D]#3D]�3D^#3D^�3D_#3D_�3D`#3D`�3Da#3Da�3Db#3Db�3Dc#3Dc�3Dd#3Dd�3De#3De�3Df#3Df�3Dg#3Dg�3Dh#3Dh�3Di#3Di�3Dj#3Dj�3Dk#3Dk�3Dl#3Dl�3Dm#3Dm�3Dn#3Dn�3Do#3Do�3Dp#3Dp�3Dq#3Dq�3Dr#3Dr�3Ds#3Ds�3Dt#3Dt�3Dy� D��D�H D�{4D���D� D�^gD�{4D���D�gD�T�D��4D��4D�!�D�Q�DڞgD��4D��D�;4D�{4D��g11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A���A���A���A���A�  A���A���A���A�  A�  A�A�%A�
=A�VA�JA�1A�VA��A��A� �A��A�{A�JA��;Aֺ^A֩�A֩�A֏\A�C�A�=qA��A�%A��Aէ�A�9XA��`A�ffA��
A�n�AГuAΧ�AͮA�`BA˶FA�1'Aʣ�Aɲ-A���Aƛ�A��A�VA�jA�A�A���A�z�A���A�ZA�r�A���A��+A�bNA��uA��A�(�A���A�\)A�jA���A���A��A��
A���A��A�  A�9XA�=qA�;dA�$�A�dZA�  A��TA��A�33A��\A��!A�+A���A�1A��A���A|-Awp�Av�Aq��Aml�Ai�PAe
=A`v�A[�7AXbAV��AU?}AR�/AOoAI�AE�7A@�+A>�RA>1'A<I�A:Q�A8��A8z�A6ffA3\)A1��A0�!A-��A+\)A*��A)��A(��A'�A&�/A%p�A#�wA#��A#�;A!O�A�A��AƨAG�A^5A��A�hA~�A  A�+AS�AA��A�`AAQ�A�A�7A7LA��A��A�AA�9A�+A��A�`A(�AdZA9XAjA\)AC�A��An�A��A�A��A�7A~�A �A�AjA
=@��@���@�
=@�-@��\@��+@�-@��@�
=@�7L@���@���@�bN@�\)@���@�/@�ȴ@�ȴA ^5A bNA ^5A ĜA �A �A ĜA I�A n�A �uA �jA%A ��A �jA �\A ~�A r�A M�A 1'A J@��@���@��h@�b@�1@�+@�E�@��-@�7L@�j@�S�@�\)@���@���@�v�@�J@��#@�hs@��j@�K�@��@���@���@�Q�@��@�@�J@��@�X@�9X@�r�@�@��@�D@�  @��@��@�
=@�o@�o@�33@�33@�;d@�;d@�C�@�+@�+@��`@�%@�j@�@�\)@�"�@��@�o@�K�@�;d@���@�-@�-@�X@�Ĝ@�j@��m@߅@�;d@�@ް!@���@���@�(�@ۅ@��@�-@ّh@���@؋D@�1@ו�@�+@�-@ա�@���@�j@�1'@Ӯ@��@��#@с@�&�@��/@Гu@Ͼw@��y@�5?@��T@͙�@�/@���@̃@��@�|�@��H@�~�@��@�x�@��@���@ǍP@�C�@�{@Ł@��@�z�@� �@��;@�o@�$�@���@�x�@�/@��@��@�dZ@�C�@��@��H@��\@��#@�?}@��9@�(�@���@�33@�{@��@��@���@��D@�z�@�j@�A�@�b@��@�|�@�K�@�+@��y@��@��h@�hs@���@���@�r�@� �@��m@��m@���@��F@�l�@��@�$�@��@��#@��@��D@�z�@�r�@�r�@�A�@��;@��@�ȴ@�ff@�$�@���@���@�X@���@��@�\)@�$�@��@�/@�Q�@���@���@�l�@�\)@�S�@�t�@�dZ@���@�@�p�@��/@���@�r�@�Q�@��@�;d@�"�@��@�ȴ@�n�@�M�@�E�@��@��h@�/@���@��j@���@��@�j@�1'@�ƨ@�dZ@�C�@�;d@�33@�"�@��R@�{@���@�G�@���@��9@��@���@���@��@� �@��@�+@��!@��+@�v�@�5?@��@���@�?}@���@��u@��m@���@���@���@���@�K�@�v�@�$�@��^@�&�@��j@��9@��@�t�@�C�@�ȴ@�v�@��@��@��#@���@��-@���@�hs@�?}@�%@��/@���@�A�@��m@���@�j@��H@��@z~�@p�@f5?@_
=@V{@Q%@H��@B-@:�\@3�@-�@&ȴ@!X@$�@ �@�/@hs@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A���A���A���A���A���A�  A���A���A���A�  A�  A�A�%A�
=A�VA�JA�1A�VA��A��A� �A��A�{A�JA��;Aֺ^A֩�A֩�A֏\A�C�A�=qA��A�%A��Aէ�A�9XA��`A�ffA��
A�n�AГuAΧ�AͮA�`BA˶FA�1'Aʣ�Aɲ-A���Aƛ�A��A�VA�jA�A�A���A�z�A���A�ZA�r�A���A��+A�bNA��uA��A�(�A���A�\)A�jA���A���A��A��
A���A��A�  A�9XA�=qA�;dA�$�A�dZA�  A��TA��A�33A��\A��!A�+A���A�1A��A���A|-Awp�Av�Aq��Aml�Ai�PAe
=A`v�A[�7AXbAV��AU?}AR�/AOoAI�AE�7A@�+A>�RA>1'A<I�A:Q�A8��A8z�A6ffA3\)A1��A0�!A-��A+\)A*��A)��A(��A'�A&�/A%p�A#�wA#��A#�;A!O�A�A��AƨAG�A^5A��A�hA~�A  A�+AS�AA��A�`AAQ�A�A�7A7LA��A��A�AA�9A�+A��A�`A(�AdZA9XAjA\)AC�A��An�A��A�A��A�7A~�A �A�AjA
=@��@���@�
=@�-@��\@��+@�-@��@�
=@�7L@���@���@�bN@�\)@���@�/@�ȴ@�ȴA ^5A bNA ^5A ĜA �A �A ĜA I�A n�A �uA �jA%A ��A �jA �\A ~�A r�A M�A 1'A J@��@���@��h@�b@�1@�+@�E�@��-@�7L@�j@�S�@�\)@���@���@�v�@�J@��#@�hs@��j@�K�@��@���@���@�Q�@��@�@�J@��@�X@�9X@�r�@�@��@�D@�  @��@��@�
=@�o@�o@�33@�33@�;d@�;d@�C�@�+@�+@��`@�%@�j@�@�\)@�"�@��@�o@�K�@�;d@���@�-@�-@�X@�Ĝ@�j@��m@߅@�;d@�@ް!@���@���@�(�@ۅ@��@�-@ّh@���@؋D@�1@ו�@�+@�-@ա�@���@�j@�1'@Ӯ@��@��#@с@�&�@��/@Гu@Ͼw@��y@�5?@��T@͙�@�/@���@̃@��@�|�@��H@�~�@��@�x�@��@���@ǍP@�C�@�{@Ł@��@�z�@� �@��;@�o@�$�@���@�x�@�/@��@��@�dZ@�C�@��@��H@��\@��#@�?}@��9@�(�@���@�33@�{@��@��@���@��D@�z�@�j@�A�@�b@��@�|�@�K�@�+@��y@��@��h@�hs@���@���@�r�@� �@��m@��m@���@��F@�l�@��@�$�@��@��#@��@��D@�z�@�r�@�r�@�A�@��;@��@�ȴ@�ff@�$�@���@���@�X@���@��@�\)@�$�@��@�/@�Q�@���@���@�l�@�\)@�S�@�t�@�dZ@���@�@�p�@��/@���@�r�@�Q�@��@�;d@�"�@��@�ȴ@�n�@�M�@�E�@��@��h@�/@���@��j@���@��@�j@�1'@�ƨ@�dZ@�C�@�;d@�33@�"�@��R@�{@���@�G�@���@��9@��@���@���@��@� �@��@�+@��!@��+@�v�@�5?@��@���@�?}@���@��u@��m@���@���@���@���@�K�@�v�@�$�@��^@�&�@��j@��9@��@�t�@�C�@�ȴ@�v�@��@��@��#@���@��-@���@�hs@�?}@�%@��/@���@�A�@��m@���@�j@��H@��@z~�@p�@f5?@_
=@V{@Q%@H��@B-@:�\@3�@-�@&ȴ@!X@$�@ �@�/@hs@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	PB	PB	PB	PB	VB	VB	VB	VB	\B	VB	bB	\B	\B	hB	uB	{B	{B	oB	�B	�B	%�B	.B	5?B	G�B	R�B	W
B	aHB	q�B	~�B	�7B	�PB	�PB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	ĜB	�BB	��B
33B
N�B
ZB
e`B
{�B
�}B
��B8RBz�B��B�RB��B�mB�B�B��B��B��B1B�B!�B+B33B5?B0!B(�B!�B�BJBB�mB�)B��B�^B��Bz�BgmBC�BB
��B
��B
�JB
YB
A�B
'�B
{B	�B	ȴB	��B	�B	� B	^5B	<jB	'�B	\B��B�B�`B�HB�/B�#B��B	PB	  B�B�B��B��B��B�B�B�TB��B��BƨBÖB��B�}BŢB��B�fB�;B�
B��B�B�yB�
BɺB��B��B�B�yB��B�B�B�B��B��B�B�B�B	�B	uB�B�)B��B��B�BB�5B�fB�`B�mB�B	,B	R�B	O�B	7LB	�B	�B	%�B	�B	�B	uB	-B	1'B	)�B	!�B	+B	@�B	@�B	<jB	:^B	5?B	/B	/B	7LB	?}B	F�B	E�B	@�B	<jB	;dB	B�B	C�B	E�B	]/B	�B	�hB	�?B	��B	��B	��B	��B	�B	�B	�B	�
B	�;B	�fB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�fB	�#B	�B	��B	��B	��B	��B	�B	�5B	�5B	�5B	�5B	�HB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B
  B
  B
  B
  B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
1B
1B
	7B

=B

=B

=B
DB
JB
JB
JB
DB
DB
DB
DB

=B
	7B
	7B

=B

=B

=B
	7B
1B
+B
B
B
B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B
DB
JB
JB
JB
JB
JB
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
hB
hB
hB
oB
oB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
(�B
2-B
8RB
?}B
D�B
K�B
Q�B
VB
YB
]/B
bNB
ffB
jB
m�B
q�B
t�B
y�B
{�B
~�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111   B	%B	#B	$B	&B	,B	,B	*B	*B	2B	,B	5B	1B	1B	:B	IB	RB	RB	EB	[B	�B	%�B	-�B	5B	G�B	R�B	V�B	aB	q}B	~�B	�B	�#B	�$B	�7B	�IB	�UB	�bB	�oB	�dB	�~B	��B	��B	�pB	�B	��B
3B
N�B
Y�B
e3B
{�B
�PB
��B8%Bz�B��B�&BϯB�?B�aB�uB��B��B��B�BpB!�B*�B3B5B/�B(�B!�BbBB �B�=B��B͢B�+B�lBz�Bg<BCcB �B
�OB
��B
�B
X�B
AUB
'�B
EB	�cB	�B	�yB	��B	�B	]�B	<1B	'�B	!B��B�oB�%B�B��B��B��B	B��B�DB�GB��B��B�~B�fB�UB�BԾB̎B�iB�WB�BB�>B�bBԿB�%B��B��BұB��B�<B��B�yB�B͑B��B�9B��B�pB�gB�pB��B�|B�UB�IB�hB	FB	3B�uB��BˆBХB�B��B�"B�B�*B�rB	+�B	R�B	O�B	7B	mB	KB	%�B	oB	CB	4B	,�B	0�B	)�B	!�B	*�B	@?B	@>B	<&B	:B	4�B	.�B	.�B	7B	?9B	FcB	E\B	@AB	<&B	;"B	BMB	CPB	E]B	\�B	��B	�%B	��B	˂B	˂B	̈B	ԺB	��B	��B	��B	��B	��B	�"B	�;B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�}B	�mB	�qB	�gB	�WB	�MB	�MB	�LB	�LB	�ZB	�tB	��B	�xB	�nB	�fB	�aB	�yB	�_B	�aB	�gB	�IB	� B	��B	��B	ПB	ΕB	ΖB	ПB	��B	��B	��B	��B	��B	�B	�'B	�8B	�AB	�AB	�SB	�SB	�SB	�SB	�QB	�KB	�RB	�WB	�XB	�YB	�fB	�wB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�wB	�qB	�mB	�]B	�^B	�mB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B

�B
B
B
B

�B

�B

�B

�B
	�B
�B
�B
	�B
	�B
	�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B

�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
!B
B
'B
(B
'B
)B
+B
*B
2B
5B
<B
@B
>B
@B
LB
KB
LB
QB
QB
SG�O�B
YB
WB
YB
`B
bB
kB
uB
uB
 }B
 |B
 |B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
(�B
1�B
8
B
?7B
DSB
K~B
Q�B
U�B
X�B
\�B
bB
fB
j6B
mHB
qbB
tuB
y�B
{�B
~�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.55 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008052019040510080520190405100805  AO  ARCAADJP                                                                    20170714000108    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170714000108  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170714000108  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100805  IP                  G�O�G�O�G�O�                