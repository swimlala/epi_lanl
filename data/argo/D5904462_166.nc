CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:52Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125952  20190405100758  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�����x�1   @���""@@/S�����d^���m1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy��D�3D�Y�D�|�D��3D�	�D�6fD�y�D��3D���D�33D�y�Dǩ�D�fD�<�D�y�D� D��fD�I�D�p D�ɚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��A��A(��AH��Ah��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB
=qB=qB=qB"=qB*=qB2=qB:=qBB=qBJ=qBR=qBZ=qBb=qBj=qBr=qBz=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �\C��C�\C�\C�\C
�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C �\C"�\C$�\C&�\C(�\C*�\C,�\C.�\C0�\C2��C4�\C6�\C8�\C:�\C<�\C>�\C@�\CB�\CD�\CF�\CH�\CJ�\CLu�CN�\CP�\CR�\CT�\CV�\CX�\CZ�\C\�\C^�\C`�\Cb�\Cd��Cf�\Ch�\Cj�\Cl�\Cn�\Cp�\Cr�\Ct�\Cv�\Cx�\Cz�\C|�\C~�\C�G�C�T{C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�T{C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�D #�D ��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D	#�D	��D
#�D
��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D #�D ��D!#�D!��D"#�D"��D##�D#��D$#�D$��D%#�D%��D&#�D&��D'#�D'��D(#�D(��D)#�D)��D*#�D*��D+#�D+��D,#�D,��D-#�D-��D.#�D.��D/#�D/��D0#�D0��D1#�D1��D2#�D2��D3#�D3��D4#�D4��D5#�D5��D6#�D6��D7#�D7��D8#�D8��D9#�D9��D:#�D:��D;#�D;��D<#�D<��D=#�D=��D>#�D>��D?#�D?��D@#�D@��DA#�DA��DB#�DB��DC#�DC��DD#�DD��DE#�DE��DF#�DF��DG#�DG��DH#�DH��DI#�DI��DJ#�DJ��DK#�DK��DL#�DL��DM#�DM��DN#�DN��DO#�DO��DP#�DP��DQ#�DQ��DR#�DR��DS#�DS��DT#�DT��DU#�DU��DV#�DV��DW#�DW��DX#�DX��DY#�DY��DZ#�DZ��D[#�D[��D\#�D\��D]#�D]��D^#�D^��D_#�D_��D`#�D`��Da#�Da��Db#�Db��Dc#�Dc��Dd#�Dd��De#�De��Df#�Df��Dg#�Dg��Dh#�Dh��Di#�Di��Dj#�Dj��Dk#�Dk��Dl#�Dl��Dm#�Dm��Dn#�Dn��Do#�Do��Dp#�Dp��Dq#�Dq��Dr#�Dr��Ds#�Ds��Dt#�Dt��Dt�qDy��D�D�k�D���D��D��D�HRD���D��D��D�ED���Dǻ�D�RD�N�Dڋ�D���D��RD�[�D��D�ۆ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�oA�oA�oA�oA�{A��A�=qA�XA�l�AΧ�AάAήA���A�AξwAάAήAζFAδ9Aΰ!A�A���A���A���A���A��/A��/A��/A��/A��#A��A��TA��A�1A��A��A��A��A�JA���A��mA���AξwAΥ�A�M�A��yAͺ^A͝�A�x�A��`A��A���A�bNA�x�A��mAˡ�A�ZA�(�A�S�A���A�\)A��A�VA�
=A�9XA���A���A�p�A�+A�ZA�%A�x�A���A�\)A��\A�x�A��!A��A��PA�oA�`BA�O�A�$�A��PA�C�A��mA�G�A�-A���A��#A���A�XA�^5A�~�A���A�x�A���A�5?A��/A�K�A�jA��A���A��hA~��Ay�Av^5At�yAq7LAj�`Ad��Ac+Aa�^Aa|�Aa"�A_�#AZ{AWx�AS��AQ�AQAP��AO�ANĜANI�AM�mAL5?AK+AI�AE?}AC��ABVA@�9A?�A?%A?G�A?dZA?"�A<z�A8��A4��A3S�A2�A0��A/A.=qA-�#A-O�A,�!A+��A+33A+�A*�A)x�A(VA'"�A&ĜA&�jA'%A&�\A%�-A$�yA#�A#`BA#hsA#\)A#�A"��A"��A"v�A"VA"E�A!A ��A ��A n�A 1'A {A�wA|�A;dAr�A��A�A|�A�A&�A�A�A��A�9A�A�AG�A�A
=A
��A
ĜA
A�A	O�A��AbNAz�A$�A��A�PAjA1'A�A�A�A�^A\)A�!AZA=qA�#A�A ��A �HA ��AVAC�A��A�AK�A7LAA �9A   @�C�@���@���@�b@���@��@��@���@�J@��@�v�@���@�K�@�@�r�@�@�v�@��@�1@�r�@�j@��;@�@��@�Ĝ@�@�D@��m@��m@�P@�|�@�bN@�9@�9@웦@���@�"�@���@�-@�x�@�G�@���@� �@�33@柾@��@�hs@��/@�1'@��;@�F@�\@�-@���@�Ĝ@��D@��@�Z@�r�@��;@�S�@��y@�-@�V@�I�@ە�@��@��@ڸR@ڟ�@�M�@�=q@�$�@�@�@�G�@ش9@�bN@� �@��@�ƨ@�K�@���@�M�@�@�Ĝ@�Z@��
@�S�@�ȴ@Ѻ^@�&�@ЋD@��m@�ff@�5?@���@͉7@�7L@���@̛�@�z�@�j@�b@�;d@�33@�
=@��y@ʇ+@�E�@��@��#@��@�j@�b@ǶF@�|�@���@�^5@�{@ũ�@��@Ĵ9@��;@�\)@���@�@�M�@�{@�J@���@�O�@��D@�1'@���@��P@�l�@��y@�E�@��h@�`B@�&�@��j@��@��@�~�@�=q@�x�@�V@���@�bN@���@�K�@�
=@��R@�^5@���@��7@�?}@�V@���@��j@��D@�Z@�(�@���@���@�33@���@��R@���@�n�@�=q@��T@���@�G�@���@�Z@��w@�l�@�"�@���@��@��!@���@�J@���@��7@���@�j@��m@���@�t�@�dZ@�S�@�33@�ȴ@�@�7L@���@�I�@��@�C�@�@��y@��R@�ff@��@���@�x�@�V@��j@�1'@��w@�|�@�o@�^5@���@���@�?}@��j@���@���@��u@�(�@��P@���@��@���@�-@��@���@��7@�hs@�O�@�G�@��@��@���@�\)@��y@���@��+@�n�@�^5@�E�@�-@�@��T@��#@��^@�p�@�G�@�&�@��@�V@���@��/@�t�@�@���@��@��9@wl�@p  @dz�@Z��@T(�@J�\@B�\@:�H@0b@+C�@&�y@"n�@�@�@�P@J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A�oA�oA�oA�oA�{A��A�=qA�XA�l�AΧ�AάAήA���A�AξwAάAήAζFAδ9Aΰ!A�A���A���A���A���A��/A��/A��/A��/A��#A��A��TA��A�1A��A��A��A��A�JA���A��mA���AξwAΥ�A�M�A��yAͺ^A͝�A�x�A��`A��A���A�bNA�x�A��mAˡ�A�ZA�(�A�S�A���A�\)A��A�VA�
=A�9XA���A���A�p�A�+A�ZA�%A�x�A���A�\)A��\A�x�A��!A��A��PA�oA�`BA�O�A�$�A��PA�C�A��mA�G�A�-A���A��#A���A�XA�^5A�~�A���A�x�A���A�5?A��/A�K�A�jA��A���A��hA~��Ay�Av^5At�yAq7LAj�`Ad��Ac+Aa�^Aa|�Aa"�A_�#AZ{AWx�AS��AQ�AQAP��AO�ANĜANI�AM�mAL5?AK+AI�AE?}AC��ABVA@�9A?�A?%A?G�A?dZA?"�A<z�A8��A4��A3S�A2�A0��A/A.=qA-�#A-O�A,�!A+��A+33A+�A*�A)x�A(VA'"�A&ĜA&�jA'%A&�\A%�-A$�yA#�A#`BA#hsA#\)A#�A"��A"��A"v�A"VA"E�A!A ��A ��A n�A 1'A {A�wA|�A;dAr�A��A�A|�A�A&�A�A�A��A�9A�A�AG�A�A
=A
��A
ĜA
A�A	O�A��AbNAz�A$�A��A�PAjA1'A�A�A�A�^A\)A�!AZA=qA�#A�A ��A �HA ��AVAC�A��A�AK�A7LAA �9A   @�C�@���@���@�b@���@��@��@���@�J@��@�v�@���@�K�@�@�r�@�@�v�@��@�1@�r�@�j@��;@�@��@�Ĝ@�@�D@��m@��m@�P@�|�@�bN@�9@�9@웦@���@�"�@���@�-@�x�@�G�@���@� �@�33@柾@��@�hs@��/@�1'@��;@�F@�\@�-@���@�Ĝ@��D@��@�Z@�r�@��;@�S�@��y@�-@�V@�I�@ە�@��@��@ڸR@ڟ�@�M�@�=q@�$�@�@�@�G�@ش9@�bN@� �@��@�ƨ@�K�@���@�M�@�@�Ĝ@�Z@��
@�S�@�ȴ@Ѻ^@�&�@ЋD@��m@�ff@�5?@���@͉7@�7L@���@̛�@�z�@�j@�b@�;d@�33@�
=@��y@ʇ+@�E�@��@��#@��@�j@�b@ǶF@�|�@���@�^5@�{@ũ�@��@Ĵ9@��;@�\)@���@�@�M�@�{@�J@���@�O�@��D@�1'@���@��P@�l�@��y@�E�@��h@�`B@�&�@��j@��@��@�~�@�=q@�x�@�V@���@�bN@���@�K�@�
=@��R@�^5@���@��7@�?}@�V@���@��j@��D@�Z@�(�@���@���@�33@���@��R@���@�n�@�=q@��T@���@�G�@���@�Z@��w@�l�@�"�@���@��@��!@���@�J@���@��7@���@�j@��m@���@�t�@�dZ@�S�@�33@�ȴ@�@�7L@���@�I�@��@�C�@�@��y@��R@�ff@��@���@�x�@�V@��j@�1'@��w@�|�@�o@�^5@���@���@�?}@��j@���@���@��u@�(�@��P@���@��@���@�-@��@���@��7@�hs@�O�@�G�@��@��@���@�\)@��y@���@��+@�n�@�^5@�E�@�-@�@��T@��#@��^@�p�@�G�@�&�@��@�V@���G�O�@�t�@�@���@��@��9@wl�@p  @dz�@Z��@T(�@J�\@B�\@:�H@0b@+C�@&�y@"n�@�@�@�P@J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
JB
1B
\B
{B
�B
�B
�B
2-B
B�B
O�B
m�B
v�B
~�B
�1B
�PB
�bB
�uB
��B
��B
�!B
�jB
ƨB
ɺB
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
�#B
�NB
�NB
�NB
�NB
�NB
�HB
�HB
�BB
�5B
�#B
��B
�}B
�LB
�3B
�B
��B"�BB�B\)Bx�B�=B��B�-BƨB�}B�-B�?B�dB�}BǮB�BB�BJBoBB��B�B��B��B��BB1B'�B8RB8RB8RB.B%�B�B�B��B�BǮB�?B��B��B�+B[#B�B
�B
�?B
��B
�B
t�B
dZB
O�B
33B
�B	�B	ÖB	��B	�JB	�=B	�=B	|�B	[#B	?}B	:^B	5?B	49B	1'B	(�B	�B	{B	hB	JB	PB	
=B	
=B	
=B	+B	B	B	  B��B	B	%B	+B	1B	bB	#�B	+B	/B	/B	�B�B�B��B��B�5B�/B�NB�HB�TB�TB�;B�yB�B�B�B�B�B�B��B	B	
=B	
=B	DB	JB	�B	�B	#�B	-B	:^B	G�B	O�B	VB	W
B	[#B	^5B	^5B	^5B	aHB	bNB	dZB	dZB	cTB	]/B	W
B	R�B	O�B	M�B	7LB	+B	$�B	!�B	�B	B�B�B�B�B�B�B�B�B�B��B��B	B	+B	1B	
=B	DB	VB	bB	�B	�B	 �B	'�B	+B	+B	-B	/B	6FB	<jB	@�B	C�B	H�B	ZB	bNB	cTB	cTB	aHB	`BB	_;B	cTB	dZB	dZB	bNB	bNB	m�B	p�B	p�B	o�B	n�B	m�B	m�B	l�B	iyB	gmB	e`B	gmB	l�B	y�B	�B	�%B	�B	�B	�B	�B	�B	�JB	�DB	�bB	�{B	��B	��B	�B	�B	�'B	�LB	�RB	�XB	�dB	�dB	�qB	�wB	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�)B	�/B	�)B	�#B	�)B	�;B	�BB	�HB	�HB	�HB	�HB	�BB	�HB	�HB	�HB	�BB	�HB	�HB	�NB	�TB	�TB	�TB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�sB	�yB	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
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
B
%B
+B
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
DB
JB
JB
JB
JB
JB
JB
JB
DB
DB
DB
DB
JB
DB
JB
JB
PB
PB
PB
PB
PB
PB
PB
VB
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
\B
bB
bB
bB
bB
bB
bB
hB
hB
oB
�B
!�B
'�B
2-B
8RB
;dB
A�B
J�B
L�B
M�B
P�B
XB
_;B
cTB
gmB
k�B
o�B
r�B
t�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111114111111111111111111111  B
B
B
.B
NB
WB
`B
�B
1�B
BcB
O�B
mdB
v�B
~�B
�B
�$B
�5B
�GB
�jB
��B
��B
�=B
�zB
ɊB
ʕB
ʕB
˝B
ͩB
άB
άB
ΪB
ήB
ήB
��B
��B
��B
� B
�!B
�#B
�#B
� B
�B
�B
�B
�B
��B
ͧB
�QB
�B
�B
��B
ͥB"�BBdB[�Bx�B�B�~B�B�zB�PB�B�B�5B�OBǀB�B�]BB?B�B��B�B�B��B��B�B�B'�B8#B8"B8!B-�B%�B�BUB��B�qB�yB�B��B�eB��BZ�BpB
�VB
�B
�KB
��B
t�B
d%B
O�B
2�B
kB	�VB	�]B	��B	�B	�B	�B	|�B	Z�B	?BB	:#B	5B	3�B	0�B	(�B	pB	@B	,B	B	B	
 B	
 B		�B	�B	�B	�B��B��B	�B	�B	�B	�B	%B	#�B	*�B	.�B	.�B	lB�xB��BѬBԿB��B��B�
B�B�B�B��B�:B�\B�iB�iB�gB�hB�uB��B	�B		�B		�B	B	B	=B	dB	#�B	,�B	:B	GlB	O�B	U�B	V�B	Z�B	]�B	]�B	]�B	aB	bB	dB	dB	cB	\�B	V�B	R�B	O�B	M�B	7B	*�B	$�B	!�B	PB	�B�tB�kB�mB�rB�qB�rB�rB�mB�lB�zB��B	�B	�B	�B		�B	
�B	B	B	<B	jB	 �B	'�B	*�B	*�B	,�B	.�B	6B	<&B	@>B	COB	HnB	Y�B	bB	cB	cB	aB	` B	^�B	cB	dB	dB	b
B	b	B	mOB	p_B	p_B	oXB	nPB	mNB	mKB	lEB	i3B	g(B	eB	g'B	lFB	y�B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�B	�6B	�dB	��B	��B	��B	��B	�B	�B	�B	� B	�!B	�+B	�0B	�;B	�HB	�XB	�\B	�cB	�hB	�nB	�|B	͍B	ΓB	ΔB	ПB	РB	ѨB	ѦB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	�B	�B	�B	��B	�B	� B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�B	�!B	�)B	�*B	�%B	�,B	�+B	�,B	�,B	�1B	�0B	�+B	�0B	�7B	�9B	�8B	�4B	�8B	�9B	�:B	�8B	�8B	�;B	�AB	�AB	�=B	�EB	�DB	�CB	�EB	�KB	�LB	�QB	�VB	�YB	�TB	�VB	�_B	�_B	�_B	�]B	�dB	�cB	�cB	�cB	�dB	�aB	�iB	�iB	�jB	�iB	�jB	�nB	�pB	�uB	�vB	�|B	�|B	�{B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
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
�B
�B
�B
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
B
B
B
B
B
B
B

�B

�B

�B

�B
B

�B
B
B
B

B
B
	B
	B
B
B
B
B
B
B
B
B
B
B
B
B
G�O�B
B
B
B
B
B
B
 G�O�B
&B
IB
!B
'�B
1�B
8	B
;B
A?B
JwB
L�B
M�B
P�B
W�B
^�B
c
B
g"B
k:B
oVB
rfB
trB
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.56 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007582019040510075820190405100758  AO  ARCAADJP                                                                    20181121125952    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125952  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125952  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100758  IP                  G�O�G�O�G�O�                