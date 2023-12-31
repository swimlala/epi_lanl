CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:57Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125957  20190405100801  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��T<1   @���zf@0vȴ9X�c���+1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dyy�D�ٚD�@ D�p D��3D�fD�@ D�p D��fD�3D�9�D�vfDǰ D��D�L�D�L�D��3D���D�L�D�|�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�u�B��)B�\B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��CnC ��C"��C$��C&��C(�HC*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�7
C�7
C�7
C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt��Dt�Dy��D��D�P�D���D��)D�'\D�P�D���D��\D�)D�J�D��\D���D�-�D�]�D�]�D��)D��D�]�D��D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�`BA�\)A�bNA�bNA�dZA�dZA�dZA�dZA�bNA�`BA�ffA�ffA�^5A�^5A�ZA�S�A��A̺^A�O�A�/A���A���A˩�A��TA�7LA�(�A��A�A���A��A�33A�(�A��A�bA���A��A�33A��Ať�A�E�A�%A��A�jA�bNA���A���A��DA�S�A�  A���A��PA�"�A�r�A���A��hA�bA�VA�hsA�t�A��HA��^A���A��#A��RA�1'A�A�^5A���A���A���A���A�ȴA�C�A��-A�n�A���A���A�O�A���A���A��PA��A�$�A�%A��`A��A�jA���A�v�A��A��HA��yA�S�A��A�A�VA���A��FA�`BA��jA��uA}33Az  Aw��AshsAnA�Aj��Ah�Aet�Ac��AahsA_A]`BAZ��AX��AWS�AVr�ATz�AQ��AM�;ALbAHA�ADv�A@��A>ffA;��A8�A8(�A7O�A5�
A3�^A2M�A1�TA05?A/�A/ƨA/��A,��A*�\A(��A%�PA$�\A%A%��A%�A%��A%�A&=qA%�A#�^A#��A#x�A#K�A#%A#C�A$9XA$�yA%S�A%`BA#�^A#oA"�A"9XA!�7A!A!%A!7LA �yA�A��AhsA7LA��A�AbA(�AVAhsA�A^5A��A  AbNA�A�A`BAA�A�A�+A$�AM�A�9Ap�A��A
�9A��AM�A��AG�A^5A�A�Ap�A`BA�A��A�7A33A5?AC�A ��A M�A 1A 1A @�;d@���@�@��^@�V@��@�^5@�G�@�1'@���@���@��`@�A�@�\)@��@�  @��@@�7L@�I�@�t�@�o@��y@��@�J@�X@�/@���@�bN@�9X@��@�v�@�J@�?}@�@�bN@� �@�ƨ@���@�@���@��/@�Ĝ@�bN@�1@߮@�S�@ޟ�@�-@݉7@�G�@���@���@ܓu@��@�ƨ@�t�@ڟ�@���@�7L@��`@���@ج@�j@�1@�S�@���@ָR@և+@�n�@��@ՙ�@�&�@Դ9@�Z@�1@�\)@�ff@�O�@��/@���@�Q�@υ@��y@�@�V@͡�@��@��m@ˮ@�S�@�o@�{@�x�@�p�@�X@�?}@�/@���@�z�@��@�|�@�+@ƸR@�ff@�$�@��T@�x�@Ĭ@ă@�r�@�1@Õ�@ÍP@î@öF@�\)@��y@�~�@�@�7L@���@�Ĝ@��D@�Z@��m@�dZ@��y@���@���@��@��m@��
@���@��w@��@�|�@�K�@�l�@�|�@�S�@�o@�ȴ@�^5@�@��-@��^@���@�X@�V@��@��@�1@���@�@�v�@�5?@���@��7@�/@�Ĝ@��u@�I�@�  @��@�ff@�M�@�=q@�$�@�@�X@��@�j@�1@�t�@�l�@�\)@���@�n�@�$�@�@���@��/@�bN@�(�@�+@��@��@�p�@�p�@���@��9@���@�Z@��@��;@�K�@��@�
=@���@�v�@�ff@�M�@�5?@�=q@�5?@��@��h@�`B@�V@�Q�@���@�;d@�33@�o@��R@��\@���@�~�@�M�@�V@��T@�@���@���@��@�X@���@�r�@�9X@��@�1@��@���@��@�S�@��H@���@���@�E�@���@���@���@�Z@�  @���@��@�~�@�^5@�V@�M�@�$�@���@���@�G�@���@��j@�  @�dZ@�
=@�ȴ@�V@�$�@�@��@��@��#@���@��^@�`B@��@��j@�I�@�1@��7@�n�@��m@K�@tj@i�#@_�;@VV@Nȴ@Gl�@@�@:�\@4j@,�/@&��@#S�@O�@�`@z�@r�@t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�`BA�\)A�bNA�bNA�dZA�dZA�dZA�dZA�bNA�`BA�ffA�ffA�^5A�^5A�ZA�S�A��A̺^A�O�A�/A���A���A˩�A��TA�7LA�(�A��A�A���A��A�33A�(�A��A�bA���A��A�33A��Ať�A�E�A�%A��A�jA�bNA���A���A��DA�S�A�  A���A��PA�"�A�r�A���A��hA�bA�VA�hsA�t�A��HA��^A���A��#A��RA�1'A�A�^5A���A���A���A���A�ȴA�C�A��-A�n�A���A���A�O�A���A���A��PA��A�$�A�%A��`A��A�jA���A�v�A��A��HA��yA�S�A��A�A�VA���A��FA�`BA��jA��uA}33Az  Aw��AshsAnA�Aj��Ah�Aet�Ac��AahsA_A]`BAZ��AX��AWS�AVr�ATz�AQ��AM�;ALbAHA�ADv�A@��A>ffA;��A8�A8(�A7O�A5�
A3�^A2M�A1�TA05?A/�A/ƨA/��A,��A*�\A(��A%�PA$�\A%A%��A%�A%��A%�A&=qA%�A#�^A#��A#x�A#K�A#%A#C�A$9XA$�yA%S�A%`BA#�^A#oA"�A"9XA!�7A!A!%A!7LA �yA�A��AhsA7LA��A�AbA(�AVAhsA�A^5A��A  AbNA�A�A`BAA�A�A�+A$�AM�A�9Ap�A��A
�9A��AM�A��AG�A^5A�A�Ap�A`BA�A��A�7A33A5?AC�A ��A M�A 1A 1A @�;d@���@�@��^@�V@��@�^5@�G�@�1'@���@���@��`@�A�@�\)@��@�  @��@@�7L@�I�@�t�@�o@��y@��@�J@�X@�/@���@�bN@�9X@��@�v�@�J@�?}@�@�bN@� �@�ƨ@���@�@���@��/@�Ĝ@�bN@�1@߮@�S�@ޟ�@�-@݉7@�G�@���@���@ܓu@��@�ƨ@�t�@ڟ�@���@�7L@��`@���@ج@�j@�1@�S�@���@ָR@և+@�n�@��@ՙ�@�&�@Դ9@�Z@�1@�\)@�ff@�O�@��/@���@�Q�@υ@��y@�@�V@͡�@��@��m@ˮ@�S�@�o@�{@�x�@�p�@�X@�?}@�/@���@�z�@��@�|�@�+@ƸR@�ff@�$�@��T@�x�@Ĭ@ă@�r�@�1@Õ�@ÍP@î@öF@�\)@��y@�~�@�@�7L@���@�Ĝ@��D@�Z@��m@�dZ@��y@���@���@��@��m@��
@���@��w@��@�|�@�K�@�l�@�|�@�S�@�o@�ȴ@�^5@�@��-@��^@���@�X@�V@��@��@�1@���@�@�v�@�5?@���@��7@�/@�Ĝ@��u@�I�@�  @��@�ff@�M�@�=q@�$�@�@�X@��@�j@�1@�t�@�l�@�\)@���@�n�@�$�@�@���@��/@�bN@�(�@�+@��@��@�p�@�p�@���@��9@���@�Z@��@��;@�K�@��@�
=@���@�v�@�ff@�M�@�5?@�=q@�5?@��@��h@�`B@�V@�Q�@���@�;d@�33@�o@��R@��\@���@�~�@�M�@�V@��T@�@���@���@��@�X@���@�r�@�9X@��@�1@��@���@��@�S�@��H@���@���@�E�@���@���@���@�Z@�  @���@��@�~�@�^5@�V@�M�@�$�@���@���@�G�@���@��j@�  @�dZ@�
=@�ȴ@�V@�$�@�@��@��@��#@���@��^@�`B@��@��j@�I�G�O�@��7@�n�@��m@K�@tj@i�#@_�;@VV@Nȴ@Gl�@@�@:�\@4j@,�/@&��@#S�@O�@�`@z�@r�@t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B"�B"�B'�BB�B�B��B��B�dB�
B�B�mB��B��B	+B	 �B	jB	�XB	�B
ZB
��B+Be`BcTBq�Bu�B{�B� B� B}�B��B�B�wBŢB�B��BB
=B{B�B�B&�B)�B1'B;dBB�BD�BE�BH�BF�BO�BH�B1'B(�B�B1B��B�B�B��BŢB�?B��B�oB� Bx�BjB[#BO�BK�BI�B<jB0!B&�B�BJB
��B
�B
�/B
��B
�dB
�hB
w�B
hsB
YB
P�B
=qB
+B
+B	�B	�BB	ÖB	��B	�1B	v�B	cTB	S�B	F�B	7LB	(�B	�B	
=B	B��B�B�mB�NB�BB�;B�TB�BB�`B	%B	JB	DB		7B	
=B	VB	�B	&�B	&�B	J�B	P�B	S�B	A�B	0!B	�B		7B	1B	�B	49B	8RB	C�B	R�B	aHB	e`B	hsB	v�B	�JB	��B	��B	�!B	��B	�B	��B	��B	�B	�B	��B	��B	��B	��B
B
DB
DB
+B
B	�B	�B	�#B	ȴB	�qB	�wB	�!B	��B	��B	��B	�-B	��B	�}B	�jB	�LB	�LB	�mB	�ZB	�5B	B	�B	��B	��B	��B	� B	t�B	q�B	o�B	r�B	r�B	s�B	t�B	t�B	y�B	z�B	{�B	|�B	z�B	w�B	r�B	p�B	s�B	v�B	~�B	�B	�+B	�%B	�1B	�=B	�DB	�=B	�=B	�1B	�+B	�+B	�=B	�JB	�JB	�PB	�oB	�{B	��B	�uB	�{B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�FB	�^B	�wB	�jB	�wB	B	ÖB	ÖB	ĜB	ĜB	ƨB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�BB	�;B	�HB	�HB	�BB	�5B	�5B	�5B	�BB	�BB	�;B	�BB	�BB	�fB	�mB	�`B	�ZB	�ZB	�ZB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�fB	�fB	�mB	�fB	�fB	�mB	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
%B
B
B
B
B
B
B
%B
+B
1B
	7B
DB
DB
JB
PB
\B
hB
hB
hB
hB
oB
oB
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
{B
{B
uB
uB
uB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
'�B
-B
49B
8RB
<jB
B�B
G�B
L�B
Q�B
W
B
[#B
`BB
ffB
iyB
l�B
q�B
u�B
y�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B!�B"�B"�B'�BBcB��B�VB��B�;B��B�WB�@B��B��B	B	 �B	jTB	�/B	�^B
Y�B
��B*�Be2Bc*Bq}Bu�B{�B�B�B}�B��B��B�HB�vB�XB��B �B
BMB`BB&�B)�B0�B;4BBcBDmBEqBH�BFyBO�BH�B0�B(�B�BB��B�nB��B˓B�pB�B��B�<B�Bx�BjNBZ�BO�BK�BI�B<9B/�B&�BnBB
��B
�MB
��B
ѵB
�0B
�4B
w�B
h?B
X�B
P�B
=8B
*�B
�B	�sB	�B	�`B	��B	��B	v�B	cB	S�B	FmB	7B	(�B	HB	
B	�B��B�uB�3B�B�B�B�B�B�%B	�B	B	B	�B	
B	B	iB	&�B	&�B	J�B	P�B	S�B	AOB	/�B	wB	�B	�B	zB	3�B	8B	CWB	R�B	a	B	e!B	h4B	v�B	�B	�OB	�aB	��B	ѭB	�TB	��B	��B	�sB	�sB	�B	��B	��B	��B
�B
B
B
�B
�B	�iB	�NB	��B	�wB	�2B	�;B	��B	��B	�eB	��B	��B	ˊB	�=B	�,B	�B	�B	�.B	�B	��B	�OB	��B	�gB	��B	�aB	�B	t}B	qjB	o^B	rnB	rpB	ssB	t{B	t}B	y�B	z�B	{�B	|�B	z�B	w�B	rnB	pbB	stB	v�B	~�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�	B	�	B	�B	�,B	�9B	�BB	�2B	�:B	�EB	�XB	�qB	��B	��B	��B	��B	��B	��B	�B	�B	�5B	�)B	�7B	�LB	�RB	�SB	�YB	�WB	�cB	�_B	�iB	̉B	ϛB	ϝB	ΗB	ΖB	͑B	͐B	ϛB	ϝB	СB	ѩB	ԼB	ԼB	ԽB	ԺB	ԻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�B	�B	� B	��B	��B	��B	��B	��B	��B	��B	��B	�"B	�)B	�B	�B	�B	�B	�B	�$B	�)B	�)B	�*B	�(B	�(B	�#B	�#B	�*B	�!B	�!B	�)B	�=B	�6B	�4B	�3B	�9B	�GB	�MB	�GB	�GB	�]B	�gB	�mB	�kB	�iB	�`B	�[B	�TB	�]B	�]B	�[B	�\B	�TB	�SB	�NB	�<B	�-B	�6B	�@B	�?B	�AB	�DB	�HB	�KB	�hB	�kB	�rB	�rB	�uB	�xB	�zB	�sB	�rB	�xB	�xB	��B	��B	��B	��B	��B	�B	�yB	�wB	�xB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B

B
B
$B
%B
!B
#B
+B
+B
4B
>B
>B
5B
=B
;B
:B
>B
;B
>B
;B
6B
8B
/B
1B
2B
(B
+B
/B
1B
1B
6B
?B
AB
=B
;B
<B
DB
AB
CB
AB
BB
FB
FB
IB
KB
GB
IB
OB
MB
UB
TG�O�B
yB
'�B
,�B
3�B
8B
<&B
BJB
GfB
L�B
Q�B
V�B
Z�B
` B
f B
i3B
lIB
qfB
u}B
y�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.53 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008012019040510080120190405100801  AO  ARCAADJP                                                                    20181121125957    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125957  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125957  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100801  IP                  G�O�G�O�G�O�                