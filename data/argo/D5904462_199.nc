CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-06-28T00:01:16Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170628000116  20190405100804  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�z,���1   @�z��v~@-a���o�dM����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�33B�  B���B�  B�33B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D�  D�<�D��3D�� D���D�VfD��3D��3D�3D�I�D�s3Dǳ3D�3D�VfDډ�D��fD���D�FfD� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�fgA��A(��AH��Ah��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B
33B33B33B"33B*33B233B:33BB33BJ33BR33BZ33Bb33Bj33Br33Bz33B��B��B��B��B�L�B�L�B��gB��B��B��B��gB��B��B��B��B��B��B��B��B��B��B��B��B��B�L�B�L�B��B�L�B��B��gB��B�L�C s3C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH�gCJ�gCL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfD #3D �3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D	#3D	�3D
#3D
�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D #3D �3D!#3D!�3D"#3D"�3D##3D#�3D$#3D$�3D%#3D%�3D&#3D&�3D'#3D'�3D(#3D(�3D)#3D)�3D*#3D*�3D+#3D+�3D,#3D,�3D-#3D-�3D.#3D.�3D/#3D/�3D0#3D0�3D1#3D1�3D2#3D2�3D3#3D3�3D4#3D4�3D5#3D5�3D6#3D6�3D7#3D7�3D8#3D8�3D9#3D9�3D:#3D:�3D;#3D;�3D<#3D<�3D=#3D=�3D>#3D>�3D?#3D?�3D@#3D@�3DA#3DA�3DB#3DB�3DC#3DC�3DD#3DD�3DE#3DE�3DF#3DF�3DG#3DG�3DH#3DH�3DI#3DI�3DJ#3DJ�3DK#3DK�3DL#3DL�3DM#3DM�3DN#3DN�3DO#3DO�3DP#3DP�3DQ#3DQ�3DR#3DR�3DS#3DS�3DT#3DT�3DU#3DU�3DV#3DV�3DW#3DW�3DX#3DX�3DY#3DY�3DZ#3DZ�3D[#3D[�3D\#3D\�3D]#3D]�3D^#3D^�3D_#3D_�3D`#3D`�3Da#3Da�3Db#3Db�3Dc#3Dc�3Dd#3Dd�3De#3De�3Df#3Df�3Dg#3Dg�3Dh#3Dh�3Di#3Di�3Dj#3Dj�3Dk#3Dk�3Dl#3Dl�3Dm#3Dm�3Dn#3Dn�3Do#3Do�3Dp#3Dp�3Dq#3Dq�3Dr#3Dr�3Ds#3Ds�3Dt#3Dt�3Dt� Dy�3D�1�D�NgD���D�њD�4D�h D���D���D�$�D�[4D���D���D��D�h Dڛ4D�� D��gD�X D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ƨA�ƨA׼jA׼jA׼jA���A׺^Aװ!Aײ-Aײ-Aק�Aק�Aן�Aי�Aי�Aי�Aי�Aכ�Aכ�Aכ�Aכ�Aכ�Aכ�Aכ�Aכ�Aכ�Aכ�Aי�Aי�Aח�A׏\A�n�A�VA�&�A�S�A�&�Aҝ�A�A�dZA�S�A�VA�1A���A�l�A���A��A�{Aɕ�A�9XA��yAȅA�/Aǣ�AƶFA�^5A�dZA�\)A�9XA���A�`BA�5?A��A���A�=qA���A�C�A��DA�A�/A��A�XA��A�I�A���A���A���A���A��A�JA�C�A���A���A��A�z�A���A�A��7A���A��!A���A{;dAuVAs%Ann�Ah^5A`1'A\�AZ��AX��AV�AR��ANz�AM�ALr�AI�
AGS�AE�wAE?}AB$�A=ƨA;�A:{A9C�A7XA6M�A5`BA4�A3�
A2��A2�!A29XA1�hA1�7A2��A2ĜA2I�A2bNA3`BA3dZA2��A1XA-?}A+��A+t�A,�A,�A,�RA)l�A( �A'�PA'�A'�;A&�+A%S�A$ �A#oA"�A"n�A!�A!|�A ��A��A��AjA�A9XA��A��A�+A1'A�A1AZA�HAt�AXAA��A~�A �At�AjA�mA�`AA�PAC�A33A��A�!AƨA�A;dA?}AC�A?}A33A��A�7A�DAƨAC�A&�A��A{Al�AVA�/A~�A��At�AK�AS�A
�yA
jA	��A	�hA	VAM�A�;A�Ax�A?}A��Av�AI�AJA�
Ax�A�A�HA�A�uAZA��Ax�AC�A"�A&�A��A��A�PA?}A �A z�A $�@���@��@��!@�V@�G�@��u@�1@�|�@�"�@�@��!@�V@��@�r�@�t�@��\@�-@��@�7L@� �@�@�!@���@��`@�@���@�\@�7@�j@�9X@���@陚@���@蛦@��m@��@�@�V@�@��@�1'@�l�@��H@��@��@��@ᙚ@��/@�bN@���@�"�@�v�@�@���@��
@�C�@���@��H@ڗ�@�{@���@��@ؓu@��
@�K�@���@�J@�?}@���@�Ĝ@ԓu@�A�@�1@��;@���@���@�O�@�V@ЋD@�1'@��@϶F@�|�@�l�@�@���@�&�@�%@���@� �@�33@ʟ�@��#@ȋD@Ǿw@�S�@��@��H@Ƈ+@�V@�J@���@�O�@���@ļj@ēu@�bN@�A�@�1'@�9X@�9X@��
@�;d@¸R@�^5@��@���@��@�r�@��@��F@��@��!@�-@�&�@��j@�bN@�  @�|�@�33@��@��\@�?}@���@���@�Z@�1'@��m@�|�@��y@�E�@��#@�`B@�r�@�9X@�(�@�C�@���@��@�@���@���@�hs@���@�A�@��@�;d@���@�-@���@�p�@�G�@�/@��@��D@��@��@��;@��@�S�@�33@��@��!@�n�@��@�?}@�7L@�%@��/@��D@�Q�@�  @���@��@�K�@��y@���@�V@�@�hs@�O�@�O�@�7L@��@��@�Ĝ@�z�@�Q�@�A�@���@��@�+@�
=@��y@��R@���@��+@�V@�=q@��@��@�7L@��@���@��9@�A�@��@���@�l�@��@���@�v�@�ff@�V@�5?@�J@�`B@��9@��u@�A�@��;@�ƨ@���@��@�~�@�E�@�-@��@��@���@��j@��@�bN@�1@��
@��@���@�t�@��H@���@�E�@��@���@�33@��T@�1'@��y@}�h@rn�@ihs@^ȴ@W�;@OK�@H  @@�9@:M�@2��@(��@ �`@33@|�@�/@�`@/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�ƨA�ƨA׼jA׼jA׼jA���A׺^Aװ!Aײ-Aײ-Aק�Aק�Aן�Aי�Aי�Aי�Aי�Aכ�Aכ�Aכ�Aכ�Aכ�Aכ�Aכ�Aכ�Aכ�Aכ�Aי�Aי�Aח�A׏\A�n�A�VA�&�A�S�A�&�Aҝ�A�A�dZA�S�A�VA�1A���A�l�A���A��A�{Aɕ�A�9XA��yAȅA�/Aǣ�AƶFA�^5A�dZA�\)A�9XA���A�`BA�5?A��A���A�=qA���A�C�A��DA�A�/A��A�XA��A�I�A���A���A���A���A��A�JA�C�A���A���A��A�z�A���A�A��7A���A��!A���A{;dAuVAs%Ann�Ah^5A`1'A\�AZ��AX��AV�AR��ANz�AM�ALr�AI�
AGS�AE�wAE?}AB$�A=ƨA;�A:{A9C�A7XA6M�A5`BA4�A3�
A2��A2�!A29XA1�hA1�7A2��A2ĜA2I�A2bNA3`BA3dZA2��A1XA-?}A+��A+t�A,�A,�A,�RA)l�A( �A'�PA'�A'�;A&�+A%S�A$ �A#oA"�A"n�A!�A!|�A ��A��A��AjA�A9XA��A��A�+A1'A�A1AZA�HAt�AXAA��A~�A �At�AjA�mA�`AA�PAC�A33A��A�!AƨA�A;dA?}AC�A?}A33A��A�7A�DAƨAC�A&�A��A{Al�AVA�/A~�A��At�AK�AS�A
�yA
jA	��A	�hA	VAM�A�;A�Ax�A?}A��Av�AI�AJA�
Ax�A�A�HA�A�uAZA��Ax�AC�A"�A&�A��A��A�PA?}A �A z�A $�@���@��@��!@�V@�G�@��u@�1@�|�@�"�@�@��!@�V@��@�r�@�t�@��\@�-@��@�7L@� �@�@�!@���@��`@�@���@�\@�7@�j@�9X@���@陚@���@蛦@��m@��@�@�V@�@��@�1'@�l�@��H@��@��@��@ᙚ@��/@�bN@���@�"�@�v�@�@���@��
@�C�@���@��H@ڗ�@�{@���@��@ؓu@��
@�K�@���@�J@�?}@���@�Ĝ@ԓu@�A�@�1@��;@���@���@�O�@�V@ЋD@�1'@��@϶F@�|�@�l�@�@���@�&�@�%@���@� �@�33@ʟ�@��#@ȋD@Ǿw@�S�@��@��H@Ƈ+@�V@�J@���@�O�@���@ļj@ēu@�bN@�A�@�1'@�9X@�9X@��
@�;d@¸R@�^5@��@���@��@�r�@��@��F@��@��!@�-@�&�@��j@�bN@�  @�|�@�33@��@��\@�?}@���@���@�Z@�1'@��m@�|�@��y@�E�@��#@�`B@�r�@�9X@�(�@�C�@���@��@�@���@���@�hs@���@�A�@��@�;d@���@�-@���@�p�@�G�@�/@��@��D@��@��@��;@��@�S�@�33@��@��!@�n�@��@�?}@�7L@�%@��/@��D@�Q�@�  @���@��@�K�@��y@���@�V@�@�hs@�O�@�O�@�7L@��@��@�Ĝ@�z�@�Q�@�A�@���@��@�+@�
=@��y@��R@���@��+@�V@�=q@��@��@�7L@��@���@��9@�A�@��@���@�l�@��@���@�v�@�ff@�V@�5?@�J@�`B@��9@��u@�A�@��;@�ƨ@���@��@�~�@�E�@�-@��@��@���@��j@��@�bN@�1@��
@��@���@�t�@��H@���@�E�@��G�O�@�33@��T@�1'@��y@}�h@rn�@ihs@^ȴ@W�;@OK�@H  @@�9@:M�@2��@(��@ �`@33@|�@�/@�`@/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	`BB	`BB	`BB	`BB	`BB	`BB	`BB	`BB	`BB	`BB	_;B	_;B	_;B	_;B	_;B	_;B	`BB	`BB	`BB	`BB	`BB	`BB	`BB	`BB	aHB	aHB	bNB	bNB	cTB	ffB	k�B	n�B	m�B	hsB	T�B	6FB	)�B	+B	33B	K�B	q�B	�B	�;B	��B
&�B
YB
�B
�oB
��B
��B
�?B
�qB
��B
�BPBA�Bv�Bv�Bz�Be`B=qB9XB@�B.B�B�B�RBÖB�B~�BW
B'�BhB
��B
�5B
�B
��B
�LB
��B
Q�B
oB	�B	��B
F�B
ZB
aHB
bNB
O�B
49B
JB	�ZB	��B	�B	�uB	q�B	H�B	5?B	-B	$�B	�B	bB	B	  B��B�B�`B�TB�BB�NB�5B�BB�BB�BB�TB�sB�B�B��B	
=B	uB	�B	�B	.B	ffB	�bB	��B	�^B	�/B	�TB	�;B	��B	��B	��B	��B	�'B	�}B	ÖB	�B	��B	�B	�FB	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	ɺB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�)B	�TB	�B	��B
PB
oB
oB
oB
oB
uB
uB
uB
VB

=B
+B
	7B
DB
DB
PB
VB
	7B
\B
�B
�B
�B
�B
�B
�B
�B
bB
PB

=B
	7B
%B
B
  B	��B	��B	��B	��B	��B	��B	��B
B
B
  B
B
+B
B
B
B
B
B
1B

=B
DB

=B

=B

=B

=B

=B
DB
DB
	7B
1B
	7B
1B
1B

=B
1B
%B
+B
+B
1B
%B
B
B
B
B
+B
%B
B
B
%B
+B
%B
%B
%B
B
B
B
B
B
B
B	��B	��B
  B
B
B
B
B
B
  B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
  B
  B
B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
1B
1B
1B
	7B

=B

=B
DB
DB
DB

=B
	7B
	7B
	7B

=B

=B

=B
	7B
	7B

=B

=B
DB
DB
JB
JB
JB
PB
PB
PB
PB
PB
JB
JB
JB
VB
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
oB
uB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
$�B
)�B
2-B
7LB
=qB
C�B
B�B
K�B
P�B
T�B
YB
`BB
dZB
k�B
q�B
t�B
v�B
x�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	`B	`B	`B	`B	`B	`B	`B	`B	`B	`B	_B	_B	_B	_B	_B	_B	`B	`B	`B	`B	`B	`B	`B	`B	aB	a!B	b%B	b"B	c)B	f9B	kYB	nnB	mgB	hGB	T�B	6B	)�B	*�B	3B	K�B	qB	��B	�B	��B
&�B
X�B
��B
�CB
��B
��B
�B
�EB
͢B
�YB&BA\Bv�Bv�Bz�Be0B=@B9)B@SB-�BbBtB�B�dB��B~�BV�B'�B4B
��B
�B
��B
ϪB
�B
�VB
Q�B
:B	�hB	��B
FtB
Y�B
aB
bB
O�B
3�B
B	�#B	�KB	��B	�=B	qrB	H|B	5B	,�B	$�B	qB	%B	�B��B��B�CB�!B�B�B�B��B�B�B�B�B�4B�YB�mB��B		�B	8B	IB	rB	-�B	f&B	�#B	�pB	�B	��B	�B	��B	ϟB	��B	�iB	�zB	��B	�<B	�XB	��B	��B	��B	�B	ΗB	ʁB	̌B	�zB	ˇB	͒B	̌B	͒B	̍B	�yB	�oB	�rB	͑B	ԾB	̌B	ΗB	ϝB	ѪB	��B	��B	�B	�OB	��B
B
-B
-B
,B
.B
3B
4B
3B
B
	�B
�B
�B
B
 B
B
B
�B
B
QB
kB
oB
vB
yB
kB
LB
 B
B
	�B
�B
�B
�B	��B	��B	��B	��B	��B	��B	��B	��B
�B
 �B	��B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
 B
	�B
	�B
	�B
	�B
	�B
B
 B
�B
�B
�B
�B
�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
 �B
 �B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�uB	�rB	�jB	�fB	�YB	�MB	�FB	�EB	�BB	�?B	�DB	�BB	�JB	�MB	�OB	�UB	�WB	�_B	�dB	�jB	�jB	�kB	�pB	�pB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
 �B	��B	��B
 �B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B

�B

�B
	�B
�B
�B
�B
	�B
	�B
	�B
�B
�B
	�B
	�B

�B

�B
B
B
B

B
B
	B

B
	B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
"B
B
"B
!B
B
'B
,B
.B
,B
.B
,B
4B
3B
:B
>B
>B
AB
?B
GB
LB
LB
LG�O�B
RB
$�B
)�B
1�B
7B
=)B
CPB
BHB
KB
P�B
T�B
X�B
_�B
dB
k:B
qbB
tuB
v�B
x�B
}�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.55 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008042019040510080420190405100804  AO  ARCAADJP                                                                    20170628000116    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170628000116  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170628000116  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100804  IP                  G�O�G�O�G�O�                