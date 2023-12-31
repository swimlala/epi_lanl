CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:45Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125945  20190405100753  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @���,�S�1   @����Q�B@0����m�d���v�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C�fC  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�3D�� D�C3D���D���D��D�FfD�s3D�� D���D�<�D���D��fD�fD�FfDڜ�D��fD�3D�33D� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �HCnC��C��C��C
��C��C��C��C��C��C�HC��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0�HC2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch�HCj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�7
C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt�Dy�D���D�T)D���D���D�*�D�W\D��)D���D��D�M�D���D��\D�\D�W\Dڭ�D��\D�)D�D)D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�\)A�C�A�1'A�oA�1A���A��mA��/A���A�ȴA�A޸RA޸RA޴9A޴9A޲-A޴9A޴9A޲-AޮAެAީ�Aީ�Aާ�Aޣ�Aޡ�Aޝ�Aޝ�Aޗ�AޅA�;dA�A�v�Aۣ�A�5?A�E�A�|�A׃A��A�S�A�1A�bNA��AҬA�/Aщ7AмjAС�AиRA���A�\)A�dZA�1'A�v�A�G�A�Aͧ�ȂhAʕ�AɼjA�33A�hsA��`A�v�A��A�M�A¾wA�;dA��A�dZA���A�VA�E�A��A���A���A�1'A��A�$�A�Q�A��DA���A��;A��HA�A�A��A�"�A���A�-A��A�33A�  A��A�|�A��hA�G�A�?}A�M�A�jA��-A�(�A��A�7LA���A�JA���A�XA��yA���A�  A�bNA��A�ZAoAy\)AuVAr  Am+Aj�uAf�Ac%A_%A[��AV��AUoAQ�AJ��AH�RAF�yAFn�AD��ABbNA@1A>�9A=ƨA<��A;�-A:�HA9�#A8JA61A5oA4ȴA2�A1�A0v�A/�A.�DA-/A+&�A)�A'&�A%;dA$��A#VA"ffA!��A�7AbNA�-A��A �A^5AO�A�A��A�A�^Al�A33A�9A�AhsA�;A%A�
Az�A��A�mA�A
=A
��A	�A	\)A�yA��A�DA�uA{AG�A;dA��A/A�!A�AC�AĜA$�A��AXAK�A �/A ��A7LAt�A �A (�@��y@�M�@�G�@��@�9X@� �@�ƨ@���@�S�@�5?@�x�@�&�@��@���@��@��@��+@�$�@��#@��@��@���@�b@�l�@�R@�5?@�^@��@���@��@�P@�+@�-@�Z@��@��@��
@�w@�K�@�K�@�+@�R@�+@�~�@�~�@�E�@�{@���@���@��@��T@��@陚@�h@�p�@�7L@�%@���@��/@�9@�r�@��
@�|�@�+@��@�@��T@�@�p�@�`B@�9@�1@�P@�+@���@◍@��@�@�`B@���@��@�Z@ߝ�@�dZ@�\)@�33@ޏ\@���@ݡ�@��/@�bN@�Q�@�ƨ@���@�=q@���@���@�x�@��@�z�@���@�|�@��H@և+@�^5@�=q@���@�@�X@���@ԛ�@�z�@�Z@�I�@�1@Ӆ@�o@ҸR@�$�@�@щ7@�hs@�&�@��`@�Ĝ@�r�@�  @ϝ�@��@Η�@��T@�O�@�Ĝ@�9X@��m@�@�V@��T@�x�@�X@�G�@�V@��/@ȴ9@��@Ǖ�@�"�@�ȴ@Ƈ+@�E�@��@š�@�X@���@ģ�@�9X@öF@�v�@�hs@�Q�@���@��m@��m@��;@��@�l�@�+@��H@���@�5?@���@�X@�&�@�V@��j@��@�1'@��@��@�o@��!@�v�@��@��h@�x�@�G�@�Q�@��
@��@�"�@��@��-@��9@�z�@��@�l�@��@���@��\@�E�@�ff@��@���@�`B@���@���@���@��j@�z�@�9X@�1'@�1@�ƨ@�"�@�v�@��T@�`B@���@���@� �@�S�@�o@��y@��!@��!@�~�@��@��^@�Ĝ@�bN@�(�@�ƨ@���@�l�@�@��!@�E�@��@���@���@�?}@��/@���@�j@��@���@�S�@�;d@�
=@��\@���@�hs@�?}@��@�V@��@�Ĝ@��u@�Ĝ@�j@�ƨ@��P@�33@���@�{@��#@���@���@�p�@���@��j@�z�@�I�@��@���@�t�@�33@���@���@���@�ff@�1'@��R@�b@���@�  @t�@i�7@`1'@Y��@Q%@I�7@@�@9�7@3��@*-@&��@"�!@�@  @ƨ@  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�\)A�C�A�1'A�oA�1A���A��mA��/A���A�ȴA�A޸RA޸RA޴9A޴9A޲-A޴9A޴9A޲-AޮAެAީ�Aީ�Aާ�Aޣ�Aޡ�Aޝ�Aޝ�Aޗ�AޅA�;dA�A�v�Aۣ�A�5?A�E�A�|�A׃A��A�S�A�1A�bNA��AҬA�/Aщ7AмjAС�AиRA���A�\)A�dZA�1'A�v�A�G�A�Aͧ�ȂhAʕ�AɼjA�33A�hsA��`A�v�A��A�M�A¾wA�;dA��A�dZA���A�VA�E�A��A���A���A�1'A��A�$�A�Q�A��DA���A��;A��HA�A�A��A�"�A���A�-A��A�33A�  A��A�|�A��hA�G�A�?}A�M�A�jA��-A�(�A��A�7LA���A�JA���A�XA��yA���A�  A�bNA��A�ZAoAy\)AuVAr  Am+Aj�uAf�Ac%A_%A[��AV��AUoAQ�AJ��AH�RAF�yAFn�AD��ABbNA@1A>�9A=ƨA<��A;�-A:�HA9�#A8JA61A5oA4ȴA2�A1�A0v�A/�A.�DA-/A+&�A)�A'&�A%;dA$��A#VA"ffA!��A�7AbNA�-A��A �A^5AO�A�A��A�A�^Al�A33A�9A�AhsA�;A%A�
Az�A��A�mA�A
=A
��A	�A	\)A�yA��A�DA�uA{AG�A;dA��A/A�!A�AC�AĜA$�A��AXAK�A �/A ��A7LAt�A �A (�@��y@�M�@�G�@��@�9X@� �@�ƨ@���@�S�@�5?@�x�@�&�@��@���@��@��@��+@�$�@��#@��@��@���@�b@�l�@�R@�5?@�^@��@���@��@�P@�+@�-@�Z@��@��@��
@�w@�K�@�K�@�+@�R@�+@�~�@�~�@�E�@�{@���@���@��@��T@��@陚@�h@�p�@�7L@�%@���@��/@�9@�r�@��
@�|�@�+@��@�@��T@�@�p�@�`B@�9@�1@�P@�+@���@◍@��@�@�`B@���@��@�Z@ߝ�@�dZ@�\)@�33@ޏ\@���@ݡ�@��/@�bN@�Q�@�ƨ@���@�=q@���@���@�x�@��@�z�@���@�|�@��H@և+@�^5@�=q@���@�@�X@���@ԛ�@�z�@�Z@�I�@�1@Ӆ@�o@ҸR@�$�@�@щ7@�hs@�&�@��`@�Ĝ@�r�@�  @ϝ�@��@Η�@��T@�O�@�Ĝ@�9X@��m@�@�V@��T@�x�@�X@�G�@�V@��/@ȴ9@��@Ǖ�@�"�@�ȴ@Ƈ+@�E�@��@š�@�X@���@ģ�@�9X@öF@�v�@�hs@�Q�@���@��m@��m@��;@��@�l�@�+@��H@���@�5?@���@�X@�&�@�V@��j@��@�1'@��@��@�o@��!@�v�@��@��h@�x�@�G�@�Q�@��
@��@�"�@��@��-@��9@�z�@��@�l�@��@���@��\@�E�@�ff@��@���@�`B@���@���@���@��j@�z�@�9X@�1'@�1@�ƨ@�"�@�v�@��T@�`B@���@���@� �@�S�@�o@��y@��!@��!@�~�@��@��^@�Ĝ@�bN@�(�@�ƨ@���@�l�@�@��!@�E�@��@���@���@�?}@��/@���@�j@��@���@�S�@�;d@�
=@��\@���@�hs@�?}@��@�V@��@�Ĝ@��u@�Ĝ@�j@�ƨ@��P@�33@���@�{@��#@���@���@�p�@���@��j@�z�@�I�@��@���@�t�@�33@���@���@���G�O�@�1'@��R@�b@���@�  @t�@i�7@`1'@Y��@Q%@I�7@@�@9�7@3��@*-@&��@"�!@�@  @ƨ@  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
e`B
ffB
ffB
ffB
ffB
e`B
e`B
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
cTB
e`B
iyB
jB
p�B
jB
[#B
O�B
:^B
�B
JB	��B	�B	�B	�B	�B
VB
hB
�B
G�B
�B
�B
��B
ÖB
�/B
�TB
�B
��BbB33B>wBC�BVBcTB�DB��B�B��BVB�B!�BA�B`BBm�B�+B�JB��B�-B��B�;B�HB�)B�B��B��B�Bk�BT�B>wB33B&�B�B��B��B��B�B�/BŢB�?B��B��B��B�BZB)�B
��B
��B
�B
�DB
y�B
e`B
R�B
G�B
@�B
1'B
VB	��B	�fB	��B	�jB	��B	�%B	gmB	P�B	8RB	&�B	DB��B�B�B��B��B�B�B�yB�fB�ZB�NB�BB�;B�NB�mB�B�yB�B�B��B	  B	1B	hB	�B	(�B	5?B	,B	2-B	0!B	/B	1'B	7LB	@�B	A�B	=qB	8RB	.B	)�B	(�B	'�B	%�B	"�B	"�B	#�B	"�B	�B	PB	B	B	  B	B	B	B	
=B	\B	bB	oB	{B	�B	�B	�B	�B	�B	%�B	49B	49B	/B	5?B	5?B	49B	33B	2-B	0!B	0!B	1'B	49B	:^B	I�B	R�B	\)B	cTB	jB	m�B	q�B	w�B	|�B	~�B	�B	�1B	�=B	�\B	�VB	�bB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�!B	�B	�B	�B	�B	��B	�B	�B	�-B	�-B	�-B	�RB	�jB	�qB	�qB	�qB	�wB	B	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�BB	�HB	�TB	�ZB	�TB	�TB	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�fB	�mB	�sB	�B	�yB	�yB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B
DB
JB
DB
DB

=B

=B
DB
DB
JB
JB
JB
JB
PB
JB
JB
JB
DB
DB
DB
DB
JB
JB
PB
VB
bB
�B
 �B
'�B
.B
5?B
;dB
C�B
H�B
M�B
R�B
XB
\)B
bNB
e`B
hsB
l�B
q�B
w�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
e8B
fAB
f?B
f=B
f<B
e8B
e6B
d/B
d/B
d2B
d2B
d4B
d5B
e5B
e8B
e5B
d3B
d3B
d3B
d4B
d3B
d4B
d4B
d3B
d3B
d0B
d0B
c,B
c-B
c,B
e4B
iSB
jTB
pwB
jTB
Z�B
O�B
:4B
~B
 B	��B	�uB	�gB	�gB	�nB
/B
<B
�B
G�B
��B
��B
�vB
�kB
�B
�'B
�TB
��B9B3B>KBCjBU�Bc(B�BиB�fB��B+BTB!�BA\B`BmcB� B�B�VB��B�VB�B�B��B��B�XB��B��BkTBT�B>EB3B&�BtB��B�B��B�B��B�sB�B��B��B�nB��BY�B)�B
��B
͟B
��B
�B
y�B
e,B
R�B
GxB
@LB
0�B
B	��B	�.B	͚B	�3B	��B	��B	g4B	P�B	8B	&�B	
B��B�uB�~B��B��B�oB�[B�=B�,B�B�B�B��B�B�0B�BB�:B�nB�kB��B��B	�B	)B	{B	(�B	5B	+�B	1�B	/�B	.�B	0�B	7B	@CB	AIB	=2B	8B	-�B	)�B	(�B	'�B	%�B	"�B	"�B	#�B	"�B	bB	B	�B	 �B��B	�B	�B	�B		�B	B	"B	0B	:B	SB	pB	}B	}B	}B	%�B	3�B	3�B	.�B	5 B	4�B	3�B	2�B	1�B	/�B	/�B	0�B	3�B	:B	IxB	R�B	[�B	cB	j=B	mLB	qiB	w�B	|�B	~�B	��B	��B	��B	�B	�B	�!B	�.B	�,B	�EB	�^B	�jB	�rB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�(B	�,B	�0B	�/B	�6B	�NB	�dB	�lB	�lB	�lB	�mB	�rB	�pB	�zB	ˇB	̉B	̉B	̊B	ΖB	ΙB	ϝB	ϜB	ѪB	ѫB	ѪB	ѪB	УB	ѫB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�!B	�"B	�)B	�-B	�+B	�*B	�$B	�*B	�.B	�>B	�7B	�3B	�+B	�*B	�)B	�0B	�.B	�1B	�/B	�0B	�/B	�1B	�0B	�8B	�5B	�6B	�0B	�0B	�7B	�8B	�=B	�?B	�=B	�>B	�9B	�BB	�GB	�GB	�FB	�GB	�IB	�NB	�PB	�NB	�NB	�XB	�TB	�NB	�[B	�ZB	�\B	�ZB	�bB	�ZB	�XB	�\B	�ZB	�bB	�fB	�gB	�fB	�eB	�bB	�aB	�_B	�hB	�eB	�gB	�gB	�jB	�dB	�YB	�_B	�`B	�iB	�hB	�kB	�lB	�mB	�tB	�yB	�B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
B
 B
 B
	�B
	�B

�B
 B
B
B
B
B
B
B
B
B

�B
 B
B

�B
B
G�O�B
B
B
fB
 B
'�B
-�B
4�B
;B
CRB
HpB
M�B
R�B
W�B
[�B
b	B
eB
h.B
lFB
qjB
w�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.53 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007532019040510075320190405100753  AO  ARCAADJP                                                                    20181121125945    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125945  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125945  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100753  IP                  G�O�G�O�G�O�                