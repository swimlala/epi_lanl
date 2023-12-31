CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:58Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125958  20190405100802  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @� �u��1   @� @y_�@-��vȴ9�c�V�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DyS3D�fD�C3D�s3D���D��D�I�D���D��fD�3D�33D�i�DǶfD��D�@ Dڀ D��fD��D�FfD�s3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@��A��A(��AH��Ah��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB
=qB=qB=qB"=qB*=qB2=qB:=qBB=qBJ=qBR=qBZ=qBb=qBj=qBr=qBz=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�BͅB��B�Q�B��B��B��B��B��B��B��B��B��B��C �\C�\C�\C�\C�\C
��C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C �\C"�\C$�\C&�\C(�\C*�\C,�\C.�\C0�\C2�\C4�\C6�\C8�\C:�\C<�\C>�\C@�\CB�\CD�\CF�\CH�\CJ�\CL�\CN�\CP�\CRu�CT�\CV�\CX�\CZ�\C\�\C^�\C`�\Cb�\Cd�\Cf�\Ch�\Cj�\Cl�\Cn�\Cp�\Cr�\Ct�\Cv�\Cx�\Cz�\C|��C~�\C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�D #�D ��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D	#�D	��D
#�D
��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D #�D ��D!#�D!��D"#�D"��D##�D#��D$#�D$��D%#�D%��D&#�D&��D'#�D'��D(#�D(��D)#�D)��D*#�D*��D+#�D+��D,#�D,��D-#�D-��D.#�D.��D/#�D/��D0#�D0��D1#�D1��D2#�D2��D3#�D3��D4#�D4��D5#�D5��D6#�D6��D7#�D7��D8#�D8��D9#�D9��D:#�D:��D;#�D;��D<#�D<��D=#�D=��D>#�D>��D?#�D?��D@#�D@��DA#�DA��DB#�DB��DC#�DC��DD#�DD��DE#�DE��DF#�DF��DG#�DG��DH#�DH��DI#�DI��DJ#�DJ��DK#�DK��DL#�DL��DM#�DM��DN#�DN��DO#�DO��DP#�DP��DQ#�DQ��DR#�DR��DS#�DS��DT#�DT��DU#�DU��DV#�DV��DW#�DW��DX#�DX��DY#�DY��DZ#�DZ��D[#�D[��D\#�D\��D]#�D]��D^#�D^��D_#�D_��D`#�D`��Da#�Da��Db#�Db��Dc#�Dc��Dd#�Dd��De#�De��Df#�Df��Dg#�Dg��Dh#�Dh��Di#�Di��Dj#�Dj��Dk#�Dk��Dl#�Dl��Dm#�Dm��Dn#�Dn��Do#�Do��Dp#�Dp��Dq#�Dq��Dr#�Dr��Ds#�Ds��Dt#�Dt��Dyw
D�(RD�UD��D���D�+�D�[�D���D��RD�D�ED�{�D��RD�+�D�Q�Dڑ�D��RD��D�XRD�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��yAП�A�-A�p�A�=qA�+A��A�oA�A���A��A��A��A��mA��;A��#A��
A���A�ȴA���Aκ^AθRAβ-AΩ�AΥ�AΥ�AΡ�AΛ�AΗ�A΁A�7LA͏\ÁA˸RA��A���A�`BAȰ!A�VAǮAǩ�A�ĜAǮA��A��A���A�7LA�A�{Aģ�A�r�A�$�A+A�E�A�E�A��A�|�A��A�Q�A���A��
A�XA�/A�A�A�{A��jA��A��
A��`A���A��`A��A��DA���A�VA�l�A��A�Q�A���A���A�
=A���A���A��A�S�A���A��A�v�A�33A��A�33A� �A��yA��wA��#A��A�Q�A�{A���A~��A|Az�AuO�AqG�Am��Aj��Ah��Ag+Adv�A_�;A]p�AY"�AUAQ�-AO��AL��AK��AIAG�FAD9XAA+A?`BA<ĜA:�\A8��A6�/A4��A3?}A1��A0�`A/VA-�;A*�A%�A$�DA$JA$�A%p�A%��A%��A$�jA%"�A$��A$�A$ZA"�`A!�#A!��A!x�A JA?}AffA�wAȴAG�AC�A�wA`BA�A�A|�A�An�A5?A��AG�A&�AȴA��A�A$�An�AA�A�A�AȴAn�AZA-Ax�AȴAz�AA�HA��A�A33A
�+A	�7A�/A�A9XAG�A�jAr�A%AI�A��A�PAVA�wA7LA 9X@�;d@�J@�x�@��D@���@�p�@�p�@��`@�Z@�A�@�I�@�(�@��@���@�ff@��@�/@�A�@�O�@��@�dZ@�v�@�x�@�j@�Q�@@@�^5@�V@��@�?}@�@�r�@�Z@�b@�1@��@�33@�+@��@���@��@�h@�G�@�j@柾@��`@�S�@�ȴ@�V@���@�?}@�Z@�9X@�Z@߶F@���@��@�p�@��@܋D@�A�@�9X@�z�@�z�@�t�@�K�@��y@�/@��@��@�$�@ղ-@���@�j@�z�@�Q�@��@�b@��@��@�1@�1@ӶF@ӍP@��m@Ӆ@�ff@��T@��T@�x�@�7L@�Ĝ@���@�l�@Ͼw@�1@д9@��#@ѡ�@�O�@��/@У�@�\)@��@�o@��H@��@�p�@̣�@��m@˾w@�33@ʏ\@�ff@�@ɺ^@�`B@ȴ9@�z�@ǥ�@��@�ff@�7L@�%@�z�@öF@�
=@���@�^5@�J@��-@��`@��u@�(�@��w@�dZ@���@�v�@�E�@��-@�?}@��`@�bN@� �@�  @��F@�K�@���@��+@�5?@�@�?}@��@�j@��
@�K�@��y@���@��+@�ff@�=q@�$�@���@�hs@�Ĝ@�9X@��P@�o@�l�@�l�@�
=@��y@���@�5?@��#@��7@���@�I�@��F@�S�@��@�ȴ@��+@�ff@��@��@�@�p�@�V@���@�j@�1'@�ƨ@�l�@��!@���@�p�@�G�@��@�j@��@���@�M�@��T@��h@�O�@��@�Ĝ@�r�@�  @��@���@�@�ff@�V@�=q@�5?@�-@��-@���@�hs@���@���@�33@���@�~�@�-@��7@��`@�Q�@�1'@��@�b@�  @��P@�+@��@�=q@�@�p�@�`B@�G�@�&�@��u@�(�@�b@���@���@�|�@�S�@�C�@�"�@�@���@�=q@��@���@�`B@�&�@���@��@�9X@���@��@�l�@�C�@�o@���@�@��y@���@���@�v�@�^5@�-@�{@��@�{@�=q@��@��@���@��@�bN@�o@��u@���@�9X@s��@i�^@b��@Yx�@L�j@D(�@=?}@5�@-?}@&�@!%@o@��@��@��@	%@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��yAП�A�-A�p�A�=qA�+A��A�oA�A���A��A��A��A��mA��;A��#A��
A���A�ȴA���Aκ^AθRAβ-AΩ�AΥ�AΥ�AΡ�AΛ�AΗ�A΁A�7LA͏\ÁA˸RA��A���A�`BAȰ!A�VAǮAǩ�A�ĜAǮA��A��A���A�7LA�A�{Aģ�A�r�A�$�A+A�E�A�E�A��A�|�A��A�Q�A���A��
A�XA�/A�A�A�{A��jA��A��
A��`A���A��`A��A��DA���A�VA�l�A��A�Q�A���A���A�
=A���A���A��A�S�A���A��A�v�A�33A��A�33A� �A��yA��wA��#A��A�Q�A�{A���A~��A|Az�AuO�AqG�Am��Aj��Ah��Ag+Adv�A_�;A]p�AY"�AUAQ�-AO��AL��AK��AIAG�FAD9XAA+A?`BA<ĜA:�\A8��A6�/A4��A3?}A1��A0�`A/VA-�;A*�A%�A$�DA$JA$�A%p�A%��A%��A$�jA%"�A$��A$�A$ZA"�`A!�#A!��A!x�A JA?}AffA�wAȴAG�AC�A�wA`BA�A�A|�A�An�A5?A��AG�A&�AȴA��A�A$�An�AA�A�A�AȴAn�AZA-Ax�AȴAz�AA�HA��A�A33A
�+A	�7A�/A�A9XAG�A�jAr�A%AI�A��A�PAVA�wA7LA 9X@�;d@�J@�x�@��D@���@�p�@�p�@��`@�Z@�A�@�I�@�(�@��@���@�ff@��@�/@�A�@�O�@��@�dZ@�v�@�x�@�j@�Q�@@@�^5@�V@��@�?}@�@�r�@�Z@�b@�1@��@�33@�+@��@���@��@�h@�G�@�j@柾@��`@�S�@�ȴ@�V@���@�?}@�Z@�9X@�Z@߶F@���@��@�p�@��@܋D@�A�@�9X@�z�@�z�@�t�@�K�@��y@�/@��@��@�$�@ղ-@���@�j@�z�@�Q�@��@�b@��@��@�1@�1@ӶF@ӍP@��m@Ӆ@�ff@��T@��T@�x�@�7L@�Ĝ@���@�l�@Ͼw@�1@д9@��#@ѡ�@�O�@��/@У�@�\)@��@�o@��H@��@�p�@̣�@��m@˾w@�33@ʏ\@�ff@�@ɺ^@�`B@ȴ9@�z�@ǥ�@��@�ff@�7L@�%@�z�@öF@�
=@���@�^5@�J@��-@��`@��u@�(�@��w@�dZ@���@�v�@�E�@��-@�?}@��`@�bN@� �@�  @��F@�K�@���@��+@�5?@�@�?}@��@�j@��
@�K�@��y@���@��+@�ff@�=q@�$�@���@�hs@�Ĝ@�9X@��P@�o@�l�@�l�@�
=@��y@���@�5?@��#@��7@���@�I�@��F@�S�@��@�ȴ@��+@�ff@��@��@�@�p�@�V@���@�j@�1'@�ƨ@�l�@��!@���@�p�@�G�@��@�j@��@���@�M�@��T@��h@�O�@��@�Ĝ@�r�@�  @��@���@�@�ff@�V@�=q@�5?@�-@��-@���@�hs@���@���@�33@���@�~�@�-@��7@��`@�Q�@�1'@��@�b@�  @��P@�+@��@�=q@�@�p�@�`B@�G�@�&�@��u@�(�@�b@���@���@�|�@�S�@�C�@�"�@�@���@�=q@��@���@�`B@�&�@���@��@�9X@���@��@�l�@�C�@�o@���@�@��y@���@���@�v�@�^5@�-@�{@��@�{@�=q@��@��@���@��@�bN@�o@��u@���@�9X@s��@i�^@b��@Yx�@L�j@D(�@=?}@5�@-?}@&�@!%@o@��@��@��@	%@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BǮB	�B	m�B

=B
N�B
e`B
aHB
`BB
z�B
�B
�\B
�'B
��B
�;B
�
B
�fB
��B#�BO�Bp�B�oB��B��B��B�qB�B�BDBoB�BhB	7B��B��B��B��B��BJB	7BB��B�B�B�B�yBɺB�FB�+Bq�BdZBM�B:^B)�B�B{BJBB+B1B1B
��B
�sB
�B
�'B
��B
|�B
gmB
J�B
8RB
"�B
bB	��B	�ZB	��B	�9B	��B	�uB	�B	gmB	G�B	9XB	�B	1B	  B��B�B�B�fB�BB�#B�B�B��B	%B	bB	�B	�B	�B	�B	�B	�B	�B	�B	1B	B	JB	0!B	?}B	O�B	ZB	XB	n�B	x�B	�B	�B	z�B	� B	�DB	�DB	�B	~�B	y�B	t�B	p�B	l�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�oB	��B	��B	��B	�bB	��B	�3B	�3B	�9B	�3B	�-B	�-B	�?B	�XB	�?B	�-B	�!B	�B	��B	��B	��B	�uB	�hB	�\B	�PB	�JB	�JB	�+B	�B	�1B	�B	�B	�B	�B	{�B	� B	}�B	x�B	u�B	r�B	o�B	jB	e`B	dZB	ffB	gmB	hsB	m�B	r�B	v�B	w�B	{�B	�B	�B	�B	�B	�bB	�hB	�VB	�PB	�VB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�^B	�XB	�3B	�B	�B	��B	��B	�B	�!B	�!B	�-B	�LB	�RB	�LB	�FB	�FB	�FB	�RB	�XB	�dB	��B	ĜB	ÖB	ƨB	ŢB	��B	�wB	�jB	�^B	�dB	�^B	�^B	�}B	ƨB	ȴB	ȴB	��B	��B	��B	�B	�B	�B	�#B	�B	�B	�#B	�/B	�5B	�/B	�)B	�)B	�5B	�NB	�fB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
B
B
+B
+B
+B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
JB
JB
PB
PB
JB
PB
PB
JB
JB
DB
JB
PB
PB
PB
VB
VB
\B
\B
VB
VB
VB
VB
VB
bB
\B
oB
hB
bB
\B
\B
bB
hB
oB
hB
bB
hB
hB
oB
oB
uB
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
 �B
�B
�B
�B
 �B
#�B
(�B
-B
5?B
8RB
?}B
E�B
J�B
Q�B
W
B
\)B
`BB
e`B
jB
n�B
r�B
w�B
z�B
~�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BǃB	�B	mfB

B
N�B
e2B
aB
`B
z�B
��B
�+B
��B
зB
�B
��B
�:B
��B#�BO�BpvB�=B�xB�vB��B�BB��B�BB>B`B8B	B��B��B��B��B��BB	B�B��B�B�XB�XB�FBɅB�B��BquBd#BM�B:,B)�B�BIBB�B�B�B�B
��B
�<B
��B
��B
�UB
|�B
g6B
J�B
8B
"�B
'B	��B	�!B	ˍB	��B	��B	�;B	��B	g3B	GsB	9B	wB	�B��B��B�\B�CB�(B�B��B�HB�RB��B	�B	"B	[B	tB	hB	[B	lB	wB	kB	QB	�B	 �B		B	/�B	?=B	O�B	Y�B	W�B	nVB	x�B	��B	��B	z�B	�B	�B	�B	��B	~�B	y�B	tzB	paB	lIB	��B	�DB	�eB	�lB	�^B	�QB	�JB	�DB	�?B	�9B	�-B	�\B	�{B	�WB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�xB	�HB	�1B	�$B	�B	�
B	�B	�B	��B	��B	��B	��B	��B	��B	��B	{�B	�B	}�B	x�B	uB	rlB	oXB	j;B	eB	dB	f B	g$B	h.B	mLB	rkB	v�B	w�B	{�B	��B	��B	��B	��B	�B	�&B	�B	�B	�B	�B	�"B	�)B	�:B	�HB	�LB	�VB	�GB	�AB	�`B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	��B	�	B	�B	�B	�CB	�UB	�PB	�`B	�[B	�CB	�1B	�#B	�B	�B	�B	�B	�4B	�^B	�oB	�kB	�{B	͊B	ӱB	սB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�CB	�{B	��B	��B	�{B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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

�B

�B
B
B
	B
B
B
B
B
B
 B

�B
B

B
	B
B
B
B
B
B
B
B
B
B
B
B
B
&B
B
B
B
B
B
B
&B
B
B
B
 B
'B
%B
+B
2B
2B
4B
0B
0B
1B
3B
1B
0B
2B
4B
6B
1B
9B
7B
8B
@B
>B
=B
>B
CB
EB
CB
FB
IB
IB
QB
QB
UB
TB
SB
UB
VB
XB
\B
\B
_B
aB
aB
bB
bB
cB
dB
nB
!�B
 |B
vB
tB
wB
 zB
#�B
(�B
,�B
4�B
8	B
?5B
EYB
JyB
Q�B
V�B
[�B
_�B
eB
j6B
nOB
rgB
w�B
z�B
~�B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.56 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008022019040510080220190405100802  AO  ARCAADJP                                                                    20181121125958    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125958  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125958  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100802  IP                  G�O�G�O�G�O�                