CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-06-23T00:01:18Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20170623000118  20190405100804  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @���f1   @�؂�9�@-� ě���d:�t�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B���B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�fD�3D�@ D�|�D��3D�	�D�,�D�vfD�ɚD��fD�VfD��fDǼ�D�3D�VfD�p D�ٚD�fD�<�D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A��A(��AH��Ah��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB
=qB=qB=qB"=qB*=qB2=qB:=qBA�BJ=qBR=qBZ=qBb=qBj��Br��Bz=qB��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��B��B��C �\C�\C�\C�\C�\C
�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C �\C"�\C$�\C&�\C(�\C*�\C,�\C.��C0�\C2�\C4�\C6�\C8�\C:�\C<�\C>�\C@�\CB�\CD�\CF�\CH�\CJ�\CL�\CN�\CP�\CR�\CT�\CV�\CX�\CZ�\C\�\C^�\C`�\Cb�\Cd�\Cf�\Ch�\Cj�\Cl�\Cn�\Cp�\Cr�\Ct�\Cv�\Cx�\Cz�\C|�\C~��C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�T{C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�T{C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�T{C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�T{C�G�C�G�C�G�C�G�C�G�C�G�D #�D ��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D	#�D	��D
#�D
��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D #�D ��D!#�D!��D"#�D"��D##�D#��D$#�D$��D%#�D%��D&#�D&��D'#�D'��D(#�D(��D)#�D)��D*#�D*��D+#�D+��D,#�D,��D-#�D-��D.#�D.��D/#�D/��D0#�D0��D1#�D1��D2#�D2��D3#�D3��D4#�D4��D5#�D5��D6#�D6��D7#�D7��D8#�D8��D9#�D9��D:#�D:��D;#�D;��D<#�D<��D=#�D=��D>#�D>��D?#�D?��D@#�D@��DA#�DA��DB#�DB��DC#�DC��DD#�DD��DE#�DE��DF#�DF��DG#�DG��DH#�DH��DI#�DI��DJ#�DJ��DK#�DK��DL#�DL��DM#�DM��DN#�DN��DO#�DO��DP#�DP��DQ#�DQ��DR#�DR��DS#�DS��DT#�DT��DU#�DU��DV#�DV��DW#�DW��DX#�DX��DY#�DY��DZ#�DZ��D[#�D[��D\#�D\��D]#�D]��D^#�D^��D_#�D_��D`#�D`��Da#�Da��Db#�Db��Dc#�Dc��Dd#�Dd��De#�De��Df#�Df��Dg#�Dg��Dh#�Dh��Di#�Di��Dj#�Dj��Dk#�Dk��Dl#�Dl��Dm#�Dm��Dn#�Dn��Do#�Do��Dp#�Dp��Dq#�Dq��Dr#�Dr��Ds#�Ds��Dt#�Dt��Du
Dy�=D�%D�Q�D���D��D��D�>�D��RD�ۆD�RD�hRD��RD�ιD�D�hRDځ�D��D�RD�N�D�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A֓uA֓uA֕�A֗�A֗�A֙�A֙�A֙�A֗�A֗�A֕�A֕�A֕�A֑hA֍PA֋DA֋DA�v�A�dZA�ffA�I�A�/Aպ^A�oA�&�AҲ-Aң�Aҡ�Aҟ�Aҝ�A�p�A���A���A�Aѩ�AыDA�l�A��HA�p�AϋDA�v�A��HA��HA��A�5?AŴ9A�M�AËDA��A�A�&�A��A��/A�JA�JA�n�A��`A��mA���A��A���A���A���A��HA�&�A���A��TA�&�A�ƨA� �A�~�A�%A��A�\)A��A�ĜA�C�A�x�A�bNA�E�A��PA���A�A��TA�(�A��PA�jA��PA�\)A�r�A��A��9A��A���A�?}A�-Az�`Aw�TAsx�ApAnM�AlQ�Ai�Ah~�Ag+Ac��A_�mA\A�AYAWC�ARbNANĜAL��AJ�/AG�AD��AC��AA�hA?
=A<(�A:  A5�FA5+A4�A3K�A2��A1�mA0�!A/A-�A,  A+�A*��A*E�A*E�A*E�A*1'A*�A*A�A)ƨA(�A)�hA*A�A*  A)�^A)�A)O�A(I�A'dZA'VA&JA%
=A#��A#"�A#%A"�A"ZA"bA!��A!G�A �A �RA �+A ^5A��AoA�+A$�A��A�RA�A�7A�\AZA��AbNA�#A`BA��A�\AE�A�wA33AoA��A�A
=A�RA��A^5A�TAƨAt�A��A�wAoA�!AQ�A;dA��A^5A�\A�-A�AXA
�yA
��A
v�A	�wA	C�A	VA{AVA�A�DAZA�TA�wA��A&�AI�A��A7LA/AȴA�mA�7Al�A+AA �A ��A ZA $�A 1@�dZ@���@�ff@��@��h@�`B@�V@�j@���@�|�@�ȴ@��@�G�@�bN@�dZ@�"�@�=q@�`B@��9@��
@���@�^@�?}@��@@�+@�@���@�7@��@�Ĝ@�A�@��@�l�@�R@��T@�X@�u@��@�"�@�!@��@噚@��@�j@�z�@�b@�l�@�
=@��y@�M�@�O�@��D@�Q�@��@�;d@ް!@�-@��@�G�@ܬ@� �@���@۾w@�|�@��H@�5?@��@ٙ�@���@�Q�@�1@�dZ@ָR@�J@թ�@���@��/@Ԭ@�bN@�1@ӍP@�+@�ȴ@�-@Ѳ-@�x�@�hs@�O�@�/@���@Л�@��m@�|�@�K�@�"�@��@Ο�@�{@�7L@��@̣�@�bN@��;@���@ʧ�@���@�hs@�?}@�V@ȣ�@�  @ǅ@�
=@Ɨ�@�-@ŉ7@���@�z�@�A�@��;@ÍP@���@�{@���@�&�@��@�t�@���@�5?@�$�@��+@�5?@��#@�X@�Ĝ@�A�@��H@�n�@�V@�-@�p�@�G�@��`@�z�@�b@�S�@���@�v�@�V@�=q@�{@�G�@��u@�r�@�Z@� �@�  @��@�l�@��R@�M�@���@���@�x�@��`@��j@���@�I�@��P@��@���@�E�@��@�@��@���@���@���@�z�@�9X@�1@���@�o@�v�@��#@��@�hs@�/@��@�(�@�1@�l�@��!@�M�@�{@��@���@�hs@�7L@�%@�  @��F@�33@�^5@�n�@�{@���@�V@�I�@��;@�l�@�+@�
=@���@��R@���@�~�@�J@�@�hs@�/@���@�z�@��;@��@��@�ȴ@�ff@�$�@���@�?}@���@�Z@�(�@��@�1@��
@��@�t�@�K�@�o@��@�~�@��@�@���@��h@��@�G�@��@��j@�z�@�A�@�@�l�@���@~5?@t�D@m��@d�j@]/@TZ@I�@A&�@;��@7
=@0��@'|�@�@��@V@�9@j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A֓uA֓uA֕�A֗�A֗�A֙�A֙�A֙�A֗�A֗�A֕�A֕�A֕�A֑hA֍PA֋DA֋DA�v�A�dZA�ffA�I�A�/Aպ^A�oA�&�AҲ-Aң�Aҡ�Aҟ�Aҝ�A�p�A���A���A�Aѩ�AыDA�l�A��HA�p�AϋDA�v�A��HA��HA��A�5?AŴ9A�M�AËDA��A�A�&�A��A��/A�JA�JA�n�A��`A��mA���A��A���A���A���A��HA�&�A���A��TA�&�A�ƨA� �A�~�A�%A��A�\)A��A�ĜA�C�A�x�A�bNA�E�A��PA���A�A��TA�(�A��PA�jA��PA�\)A�r�A��A��9A��A���A�?}A�-Az�`Aw�TAsx�ApAnM�AlQ�Ai�Ah~�Ag+Ac��A_�mA\A�AYAWC�ARbNANĜAL��AJ�/AG�AD��AC��AA�hA?
=A<(�A:  A5�FA5+A4�A3K�A2��A1�mA0�!A/A-�A,  A+�A*��A*E�A*E�A*E�A*1'A*�A*A�A)ƨA(�A)�hA*A�A*  A)�^A)�A)O�A(I�A'dZA'VA&JA%
=A#��A#"�A#%A"�A"ZA"bA!��A!G�A �A �RA �+A ^5A��AoA�+A$�A��A�RA�A�7A�\AZA��AbNA�#A`BA��A�\AE�A�wA33AoA��A�A
=A�RA��A^5A�TAƨAt�A��A�wAoA�!AQ�A;dA��A^5A�\A�-A�AXA
�yA
��A
v�A	�wA	C�A	VA{AVA�A�DAZA�TA�wA��A&�AI�A��A7LA/AȴA�mA�7Al�A+AA �A ��A ZA $�A 1@�dZ@���@�ff@��@��h@�`B@�V@�j@���@�|�@�ȴ@��@�G�@�bN@�dZ@�"�@�=q@�`B@��9@��
@���@�^@�?}@��@@�+@�@���@�7@��@�Ĝ@�A�@��@�l�@�R@��T@�X@�u@��@�"�@�!@��@噚@��@�j@�z�@�b@�l�@�
=@��y@�M�@�O�@��D@�Q�@��@�;d@ް!@�-@��@�G�@ܬ@� �@���@۾w@�|�@��H@�5?@��@ٙ�@���@�Q�@�1@�dZ@ָR@�J@թ�@���@��/@Ԭ@�bN@�1@ӍP@�+@�ȴ@�-@Ѳ-@�x�@�hs@�O�@�/@���@Л�@��m@�|�@�K�@�"�@��@Ο�@�{@�7L@��@̣�@�bN@��;@���@ʧ�@���@�hs@�?}@�V@ȣ�@�  @ǅ@�
=@Ɨ�@�-@ŉ7@���@�z�@�A�@��;@ÍP@���@�{@���@�&�@��@�t�@���@�5?@�$�@��+@�5?@��#@�X@�Ĝ@�A�@��H@�n�@�V@�-@�p�@�G�@��`@�z�@�b@�S�@���@�v�@�V@�=q@�{@�G�@��u@�r�@�Z@� �@�  @��@�l�@��R@�M�@���@���@�x�@��`@��j@���@�I�@��P@��@���@�E�@��@�@��@���@���@���@�z�@�9X@�1@���@�o@�v�@��#@��@�hs@�/@��@�(�@�1@�l�@��!@�M�@�{@��@���@�hs@�7L@�%@�  @��F@�33@�^5@�n�@�{@���@�V@�I�@��;@�l�@�+@�
=@���@��R@���@�~�@�J@�@�hs@�/@���@�z�@��;@��@��@�ȴ@�ff@�$�@���@�?}@���@�Z@�(�@��@�1@��
@��@�t�@�K�@�o@��@�~�@��@�@���@��h@��@�G�@��@��j@�z�G�O�@�@�l�@���@~5?@t�D@m��@d�j@]/@TZ@I�@A&�@;��@7
=@0��@'|�@�@��@V@�9@j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	6FB	6FB	6FB	6FB	6FB	6FB	6FB	6FB	6FB	7LB	7LB	7LB	7LB	7LB	7LB	7LB	7LB	7LB	8RB	8RB	8RB	9XB	9XB	6FB	49B	7LB	=qB	>wB	A�B	H�B	S�B	]/B	_;B	`BB	aHB	aHB	aHB	iyB	n�B	q�B	q�B	ȴB
B
/B
P�B
�DB
��B
�fB
�sBBE�BN�B\)B�B��B��B��B��B��B�?B�/B�B��B%BVBJBB��B��B��B�B�;BÖB��B�\Bv�Be`BP�B@�B0!B#�BJB
�TB
ƨB
�-B
��B
�hB
}�B
q�B
gmB
[#B
R�B
=qB
$�B	�fB	�B	�JB	�+B	t�B	`BB	w�B	k�B	\)B	W
B	Q�B	K�B	&�B	oB	�B	uB	%B��B��B��B�B�HB�)B�B�#B�NB�ZB�HB�HB�TB�NB�BB�BB�ZB�sB�B��B��B��B	�B	'�B	.B	:^B	D�B	T�B	aHB	dZB	~�B	��B	��B	�B	�'B	�'B	�-B	�LB	�FB	�dB	�qB	ÖB	ŢB	ŢB	��B	��B	��B	�
B	�B	�BB	�NB	�ZB	�`B	�sB	�B	�B	��B	��B
%B
	7B
	7B
1B
%B
1B
JB
\B
bB
bB
oB
uB
{B
�B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
\B

=B
DB
DB
\B
bB
�B
�B
�B
�B
{B
\B

=B
	7B
B
JB
1B
B
B
B
B
B
B
B
B
B
+B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
  B
  B
  B	��B	��B	��B	��B
  B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
1B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB
JB
JB
JB
PB
PB
VB
VB
\B
\B
VB
VB
VB
PB
PB
PB
JB
VB
hB
hB
\B
VB
PB
PB
VB
\B
bB
hB
hB
hB
hB
oB
oB
oB
uB
uB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
.B
6FB
<jB
B�B
F�B
J�B
M�B
S�B
YB
\)B
`BB
cTB
iyB
o�B
t�B
x�B
|�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B	6B	6B	6B	6B	6B	6B	6B	6B	6B	7 B	7"B	7 B	7"B	7 B	7 B	7 B	7 B	7!B	8%B	8$B	8&B	9,B	9/B	6B	4B	7 B	=EB	>MB	A`B	H�B	S�B	]B	_B	`B	aB	aB	aB	iKB	nhB	q}B	q{B	ȆB
�B
.�B
P�B
�B
ѼB
�8B
�DB�BEsBN�B[�B��B��B�vB�uB��B��B�B��B�zB��B�B%BB�B��B��B��B�XB�	B�dB��B�)Bv�Be+BP�B@LB/�B#�BB
�B
�vB
��B
��B
�1B
}�B
qtB
g6B
Z�B
R�B
=7B
$�B	�0B	��B	�B	��B	t�B	`B	w�B	kJB	[�B	V�B	Q�B	K�B	&�B	5B	]B	8B	�B��B��B�B�IB�B��B��B��B�B�B�	B�B�B�B� B� B�B�4B�`B��B��B��B	eB	'�B	-�B	:B	D[B	T�B	aB	dB	~�B	�RB	��B	��B	��B	��B	��B	�
B	�B	�!B	�0B	�UB	�`B	�aB	͑B	үB	ԾB	��B	��B	�B	�B	�B	�!B	�1B	�MB	�aB	��B	��B
�B
�B
�B
�B
�B
�B
	B
B
"B
B
-B
1B
9B
>B
;B
3B
>B
IB
DB
EB
VB
]B
UB
OB
PB
<B
/B
#B
B
	�B

�B
 B
B
 B
>B
JB
CB
=B
:B
B
	�B
�B
�B
B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�~B	�B	�}B	�vB	�uB	�vB	�uB	�uB	�vB	�qB	�pB	�qB	�tB	�qB	�jB	�lB	�hB	�kB	�jB	�kB	�iB	�pB	�vB	�pB	�vB	�uB	�rB	�hB	�cB	�jB	�jB	�hB	�kB	�jB	�dB	�dB	�eB	�iB	�kB	�jB	�rB	�pB	�nB	�tB	�wB	�uB	�uB	�vB	�rB	�sB	�tB	�oB	�uB	�sB	�vB	�uB	�uB	�uB	�pB	�oB	�pB	�nB	�oB	�wB	�uB	�wB	�tB	�vB	�vB	�xB	�tB	�|B	�}B	�}B	�sB	�vB	�}B	�~B	�~B	�~B	�}B	�zB	��B	�}B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�tB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B

�B

�B

�B
B
 B
B
	B
B
B
B
B
B
	B
B
B
B
	B
B
B
B
B
#B
B
B
B
	B
B
B
B
B
B
B
B
(B
%B
(B
*B
.B
.B
3B
0B
1B
2B
3B
2B
9B
8B
>B
<B
=B
=B
>B
<B
EB
FB
FB
CB
DB
DB
DB
KB
PB
RB
VB
XB
XB
XG�O�B
pB
&�B
-�B
5�B
< B
BHB
F^B
JzB
M�B
S�B
X�B
[�B
_�B
c	B
i/B
oRB
trB
x�B
|�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.56 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008042019040510080420190405100804  AO  ARCAADJP                                                                    20170623000118    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170623000118  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170623000118  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100804  IP                  G�O�G�O�G�O�                