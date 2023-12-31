CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-07-29T17:02:49Z creation      
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
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20170729170249  20190405100805  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�`ʆRL1   @�b�	>@,�C��%�dw�z�H1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR�CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy� D�  D�33D�l�D��fD�3D�9�D���D�� D� D�C3D�|�D�� D���D�C3Dڠ D��fD�3D�FfD�p D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @]p�@��R@��A��A(��AH��Ah��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB
=qB=qB=qB"=qB*=qB2=qB:=qBB=qBJ=qBR=qBZ=qBb=qBj=qBr=qBz=qB��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �\C�\C�\C�\C�\C
�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C �\C"�\C$�\C&�\C(�\C*�\C,�\C.�\C0�\C2�\C4�\C6�\C8�\C:�\C<�\C>�\C@�\CB�\CD�\CF�\CH�\CJ�\CL�\CN�\CP��CR��CTu�CV�\CX�\CZ�\C\�\C^�\C`�\Cb�\Cd�\Cf�\Ch�\Cj�\Cl�\Cn�\Cp�\Cr�\Ct�\Cv�\Cx�\Cz�\C|�\C~�\C�G�C�G�C�G�C�G�C�T{C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�:�C�G�C�G�C�G�C�G�C�G�C�T{C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�D #�D ��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D	#�D	��D
#�D
��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D #�D ��D!#�D!��D"#�D"��D##�D#��D$#�D$��D%#�D%��D&#�D&��D'#�D'��D(#�D(��D)#�D)��D*#�D*��D+#�D+��D,#�D,��D-#�D-��D.#�D.��D/#�D/��D0#�D0��D1#�D1��D2#�D2��D3#�D3��D4#�D4��D5#�D5��D6#�D6��D7#�D7��D8#�D8��D9#�D9��D:#�D:��D;#�D;��D<#�D<�qD=#�D=��D>#�D>��D?#�D?��D@#�D@��DA#�DA��DB#�DB��DC#�DC��DD#�DD��DE#�DE��DF#�DF��DG#�DG��DH#�DH��DI#�DI��DJ#�DJ��DK#�DK��DL#�DL��DM#�DM��DN#�DN��DO#�DO��DP#�DP��DQ#�DQ��DR#�DR��DS#�DS��DT#�DT��DU#�DU��DV#�DV��DW#�DW��DX#�DX��DY#�DY��DZ#�DZ��D[#�D[��D\#�D\��D]#�D]��D^#�D^��D_#�D_��D`#�D`��Da#�Da��Db#�Db��Dc#�Dc��Dd#�Dd��De#�De��Df#�Df��Dg#�Dg��Dh#�Dh��Di#�Di��Dj#�Dj��Dk#�Dk��Dl#�Dl��Dm#�Dm��Dn#�Dn��Do#�Do��Dp#�Dp��Dq#�Dq��Dr#�Dr��Ds#�Ds��Dt#�Dt��Dt�qDy��D��D�ED�~�D��RD�%D�K�D���D���D�!�D�UD���D���D��D�UDڱ�D��RD�%D�XRD��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��A��A� �A�"�A� �A��A�bA�bA�oA�bA��A�{A�bA�1A�A���A���A��A��
A�ĜA���A���AڼjAں^Aں^AڸRAڸRAڴ9Aڲ-Aڴ9Aڴ9Aڰ!Aک�Aڡ�Aڛ�AړuAڅA�l�A�S�A�VA�x�A��A�ZA��A�x�A�jA���A�ĜA�ĜA�p�A�ƨA���A�n�AʶFA��`A�dZA�A�AǋDA�1A�~�AŸRAę�AÏ\A£�A��TA��mA�x�A��A��9A��7A�JA�~�A�hsA�t�A�~�A��mA�;dA�ZA���A�"�A� �A�"�A���A���A�
=A�-A��A��mA���A�K�A���A��mA�VA�+A��A��\A�K�A�hsA��A�;dA��^A�XA��PA�/Aw��An��Aj9XAfjAb �A_�A]��A]\)A\A�A[AZ{AY\)AW�FAU��AQ�-AN�uAN=qAM��AM?}AJAG�AF�AD  A@bA<jA;�A;��A;p�A9x�A7K�A41'A1��A0��A/��A/�A.�A-dZA,I�A(��A'
=A&VA%��A%�A${A#`BA!�A  �A7LA�+AM�A=qA-A�Al�A�`A��A~�A(�A�7A��Ap�A��A��A��A�FA1An�A��A��A��A��A�A&�A�RAbA��AE�A=qA�FA-AS�AK�Ar�A�AoAVA
^5A	oA	A�mAƨA�hA��A �A��A�#A �jA �A ��A 5?@�ȴ@�?}@��@��m@���@�E�@��^@�J@�&�@�t�@��y@��^@���@��9@�@�n�@���@��@�33@��@�@�9@�r�@�(�@�F@�33@�~�@���@��@��/@�z�@��
@�S�@�G�@��@���@�@�j@� �@�|�@�\)@��y@◍@◍@�M�@�-@��@�7@�`B@�G�@�V@�1'@��@߅@�v�@�M�@�M�@�$�@��T@݁@܋D@��H@�^5@�-@���@١�@�`B@�V@�  @�@���@�hs@�7L@�%@ԋD@���@ӕ�@���@�@�O�@���@�1'@�9X@�1'@�1@���@υ@�;d@��@��@�@�ȴ@Η�@·+@�E�@��@��T@�x�@�Z@ˍP@�l�@�@���@��@��H@�
=@�+@�"�@���@�n�@��@ɑh@��@���@�r�@�(�@��;@ǍP@�"�@��@�=q@ŉ7@��`@ļj@�1'@�\)@���@�^5@�E�@���@�hs@���@���@�r�@�  @��;@�t�@��@�n�@���@�7L@�Z@��m@��@��@�l�@�+@���@�n�@�E�@�5?@���@���@���@��u@�A�@�1@��@���@�ff@�$�@��-@��@���@��@�I�@��@��;@��@�C�@��y@�ff@�{@�@���@�%@��@�Z@�ƨ@�K�@�;d@�+@���@��+@�{@���@�p�@�O�@�%@���@�z�@�j@�9X@��@�K�@��@��R@�~�@�=q@��@��^@�`B@��u@��@���@�o@���@���@��@���@�K�@��@��\@�=q@�J@��^@��7@�p�@�V@���@��9@�j@� �@��@�l�@�\)@�33@�
=@�ȴ@���@���@�V@���@��@�O�@�z�@� �@�b@��m@��@�t�@�+@��H@�~�@��@���@��h@���@��u@�j@�A�@�  @��F@��H@��+@���@��#@�@���@��h@�X@���@���@��D@� �@��@��@�l�@�K�@�;d@�;d@��@���@�v�@�=q@���@�@���@��@�x�@�p�@�hs@�G�@�%@�Ĝ@�n�@���@��@��@w�@o�P@g\)@]�-@W�@Q��@I�@B-@9G�@4�D@.ff@(A�@$I�@\)@�@�@X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A���A��A��A� �A�"�A� �A��A�bA�bA�oA�bA��A�{A�bA�1A�A���A���A��A��
A�ĜA���A���AڼjAں^Aں^AڸRAڸRAڴ9Aڲ-Aڴ9Aڴ9Aڰ!Aک�Aڡ�Aڛ�AړuAڅA�l�A�S�A�VA�x�A��A�ZA��A�x�A�jA���A�ĜA�ĜA�p�A�ƨA���A�n�AʶFA��`A�dZA�A�AǋDA�1A�~�AŸRAę�AÏ\A£�A��TA��mA�x�A��A��9A��7A�JA�~�A�hsA�t�A�~�A��mA�;dA�ZA���A�"�A� �A�"�A���A���A�
=A�-A��A��mA���A�K�A���A��mA�VA�+A��A��\A�K�A�hsA��A�;dA��^A�XA��PA�/Aw��An��Aj9XAfjAb �A_�A]��A]\)A\A�A[AZ{AY\)AW�FAU��AQ�-AN�uAN=qAM��AM?}AJAG�AF�AD  A@bA<jA;�A;��A;p�A9x�A7K�A41'A1��A0��A/��A/�A.�A-dZA,I�A(��A'
=A&VA%��A%�A${A#`BA!�A  �A7LA�+AM�A=qA-A�Al�A�`A��A~�A(�A�7A��Ap�A��A��A��A�FA1An�A��A��A��A��A�A&�A�RAbA��AE�A=qA�FA-AS�AK�Ar�A�AoAVA
^5A	oA	A�mAƨA�hA��A �A��A�#A �jA �A ��A 5?@�ȴ@�?}@��@��m@���@�E�@��^@�J@�&�@�t�@��y@��^@���@��9@�@�n�@���@��@�33@��@�@�9@�r�@�(�@�F@�33@�~�@���@��@��/@�z�@��
@�S�@�G�@��@���@�@�j@� �@�|�@�\)@��y@◍@◍@�M�@�-@��@�7@�`B@�G�@�V@�1'@��@߅@�v�@�M�@�M�@�$�@��T@݁@܋D@��H@�^5@�-@���@١�@�`B@�V@�  @�@���@�hs@�7L@�%@ԋD@���@ӕ�@���@�@�O�@���@�1'@�9X@�1'@�1@���@υ@�;d@��@��@�@�ȴ@Η�@·+@�E�@��@��T@�x�@�Z@ˍP@�l�@�@���@��@��H@�
=@�+@�"�@���@�n�@��@ɑh@��@���@�r�@�(�@��;@ǍP@�"�@��@�=q@ŉ7@��`@ļj@�1'@�\)@���@�^5@�E�@���@�hs@���@���@�r�@�  @��;@�t�@��@�n�@���@�7L@�Z@��m@��@��@�l�@�+@���@�n�@�E�@�5?@���@���@���@��u@�A�@�1@��@���@�ff@�$�@��-@��@���@��@�I�@��@��;@��@�C�@��y@�ff@�{@�@���@�%@��@�Z@�ƨ@�K�@�;d@�+@���@��+@�{@���@�p�@�O�@�%@���@�z�@�j@�9X@��@�K�@��@��R@�~�@�=q@��@��^@�`B@��u@��@���@�o@���@���@��@���@�K�@��@��\@�=q@�J@��^@��7@�p�@�V@���@��9@�j@� �@��@�l�@�\)@�33@�
=@�ȴ@���@���@�V@���@��@�O�@�z�@� �@�b@��m@��@�t�@�+@��H@�~�@��@���@��h@���@��u@�j@�A�@�  @��F@��H@��+@���@��#@�@���@��h@�X@���@���@��D@� �@��@��@�l�@�K�@�;d@�;d@��@���@�v�@�=q@���@�@���@��@�x�@�p�@�hs@�G�@�%G�O�@�n�@���@��@��@w�@o�P@g\)@]�-@W�@Q��@I�@B-@9G�@4�D@.ff@(A�@$I�@\)@�@�@X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	ɺB	ȴB	ȴB	ȴB	ȴB	ȴB	ȴB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ƨB	ƨB	ƨB	ŢB	ŢB	ĜB	ÖB	B	B	B	B	��B	��B	��B	��B	�}B	��B	��B	��B	�}B	�wB	�qB	�jB	�^B	�LB	��B	�B	��B	��B	��B	�
B	��B	��B
	7B
49B
_;B
k�B
�B
��B
�3B
��B
�mB
�B\B �B,B5?B@�BR�BffBv�B�=B��B�-BBȴBɺB��B�ZB��B��BBhB�B�B�B��B��B�jB�'B�DBq�B\)B-B  B
�B
�sB
�B
�B
�B
��B
�?B
�-B
�B
�uB
`BB
E�B
.B
"�B	��B	��B	��B	o�B	ZB	H�B	7LB	-B	(�B	%�B	 �B	�B	�B	�B	{B	bB	PB	JB	DB		7B	%B	+B	%B	B	B	%B	oB	uB	uB	{B	#�B	/B	B�B	L�B	O�B	R�B	VB	[#B	^5B	aHB	t�B	x�B	w�B	x�B	y�B	v�B	v�B	r�B	m�B	hsB	jB	n�B	p�B	o�B	jB	ffB	iyB	k�B	k�B	jB	gmB	e`B	aHB	e`B	ffB	jB	q�B	e`B	\)B	\)B	]/B	bNB	_;B	VB	L�B	H�B	H�B	P�B	`BB	aHB	S�B	hsB	��B	��B	��B	��B	��B	�DB	�B	}�B	}�B	aHB	gmB	n�B	q�B	p�B	p�B	n�B	hsB	l�B	l�B	s�B	r�B	o�B	p�B	p�B	p�B	q�B	p�B	v�B	v�B	s�B	u�B	v�B	x�B	{�B	|�B	~�B	�B	�B	�B	�%B	�7B	�PB	�VB	�VB	�VB	�\B	�bB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�9B	�9B	�FB	�XB	�^B	�dB	�dB	�jB	�wB	�wB	�qB	�}B	�}B	�}B	��B	��B	��B	��B	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�HB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�TB	�NB	�HB	�NB	�`B	�fB	�mB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
  B
  B
  B
  B
  B
  B
B
B
B
B
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
+B
%B
+B
+B
+B
+B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B
	7B
	7B

=B
DB
JB
JB
PB
PB
PB
PB
PB
VB
VB
PB
JB
DB
	7B
%B
B
B
%B
+B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B
JB
PB
PB
JB
JB
PB
VB
bB
oB
uB
oB
uB
uB
uB
oB
oB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
!�B
$�B
.B
5?B
:^B
C�B
F�B
M�B
P�B
T�B
YB
^5B
`BB
e`B
hsB
jB
o�B
t�B
t�B
x�B
{�1111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B	ɏB	ȉB	ȆB	ȈB	ȈB	ȋB	ȆB	ǂB	ǂB	ǁB	ǂB	ǃB	ǂB	ǁB	�{B	�~B	�B	�sB	�xB	�rB	�iB	�dB	�dB	�fB	�fB	�[B	�^B	�`B	�WB	�RB	�WB	�WB	�WB	�UB	�JB	�HB	�@B	�2B	�"B	�{G�O�B	�hB	�pB	��B	��B	��B	��B
	B
4B
_B
kYB
��B
��B
�B
̢B
�>B
�B.B �B+�B5B@UBR�Bf5Bv�B�B��B��B�_BȅBɊB˖B�)B��B��B �B8BpB�BOB��BʏB�:B��B�BqxB[�B,�B
��B
�{B
�>B
��B
��B
��B
��B
�
B
��B
��B
�?B
`	B
ElB
-�B
"�B	��B	ҹB	�\B	obB	Y�B	HxB	7B	,�B	(�B	%�B	 �B	uB	jB	UB	AB	'B	B	B	B	�B	�B	�B	�B	�B	�B	�B	3B	7B	5B	>B	#�B	.�B	BNB	L�B	O�B	R�B	U�B	Z�B	]�B	aB	tzB	x�B	w�B	x�B	y�B	v�B	v�B	rmB	mPB	h3B	j?B	nVB	pgB	o[B	j>B	f%B	i7B	kBB	kCB	j>B	g*B	eB	aB	eB	f"B	j=B	qgB	eB	[�B	[�B	\�B	bB	^�B	U�B	L�B	HpB	HmB	P�B	_�B	aB	S�B	h/B	�mB	��B	��B	�NB	�VB	�B	��B	}�B	}�B	aB	g&B	nSB	qdB	p_B	paB	nQB	h,B	lGB	lHB	spB	rlB	oWB	p_B	p`B	p^B	qeB	p]B	v�B	v�B	soB	uB	v�B	x�B	{�B	|�B	~�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�*B	�;B	�@B	�ZB	�fB	�nB	�nB	�wB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�/B	�0B	�+B	�6B	�6B	�4B	�<B	�=B	�BB	�AB	�[B	�_B	�{B	�{B	�xB	�B	ˀB	̅B	ˀB	̆B	ΒB	ΔB	ϕB	ӱB	ռB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	�?B	�TB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�zB	�uB	�pB	�nB	�uB	�yB	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B

B
B
B
B
	B
B
B
B
B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
B
B
B
B
B
B
B
B
$B
-B
'B
.B
-B
-B
'B
(B
(B
%B
'B
#B
+B
*B
+B
+B
*B
(B
7B
9B
=B
>B
>B
>B
>B
DB
KB
LB
JB
QB
RB
VB
WB
ZB
]B
]B
VB
]B
bB
bB
gB
gB
jB
jB
jB
iB
kB
iB
jG�O�B
!�B
$�B
-�B
4�B
:B
CMB
F_B
M�B
P�B
T�B
X�B
]�B
_�B
eB
h*B
j6B
oUB
tsB
ttB
x�B
{�1111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.56 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008052019040510080520190405100805  AO  ARCAADJP                                                                    20170729170249    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170729170249  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170729170249  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100805  IP                  G�O�G�O�G�O�                