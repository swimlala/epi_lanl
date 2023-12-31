CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:47Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125947  20190405100755  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @���CQ�u1   @�����@0B��`A��d��"��`1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C�C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy�fD�fD�@ D��fD�� D��fD�9�D�vfD�ɚD���D�C3D�|�D�� D� D�P D�y�D��fD��fD�L�D�fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�B�B�\B�\B�\B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�B�B�\B�\B�\B�\B�\B�\B�\C ��C��C��C�HC��C
��C��C��C��C��C��C��C��C�HC�HC��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb�HCd�HCf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt��Dt��Dy�RD�'\D�P�D��\D���D��\D�J�D��\D�ڐD��D�T)D���D���D� �D�`�Dڊ�D��\D�\D�]�D�\D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�hA�hA�hA�uA�uA╁A╁A◍A╁A╁A╁A╁A◍A♚A♚A◍A◍A╁A╁A◍A◍A◍A◍A◍A�hA�\A�\A�hA�\A�~�A�hsA�C�A�A�p�A߉7A�5?Aݰ!A��A��yAװ!AՃA��A���A��;AԓuA�`BA�  A�+A���A�VA��A�/A�-A�33A�oA���A�E�A̕�A˶FAʮA�z�A�^5AǴ9AǕ�A���A���A�dZA�9XA�XA�l�A��
A��HA�I�A��A�\)A��A���A�bNA�=qA�^5A��A�t�A��A���A�1A�z�A��hA���A�5?A��yA�=qA�ffA�XA�5?A���A��A�ȴA��A��A���A��DA�\)A�$�A��hA�ȴA�VA��FA�S�A�  A�(�Ayp�As��Al�\Ai��A^�`AZ�DAY;dAW&�AR�AM+AH��AG�TAGoAE��AD-AB��A??}A<ZA;C�A:�A8�HA7�#A6�A41A3�A3VA2�RA0�/A0ZA/�^A/
=A-��A,��A+�mA*�A(^5A'/A%ƨA%+A$��A$1A#�
A$�\A#��A#7LA"I�A �A�A(�A�A��A�
A��A�wA  Ar�A�An�AƨA33AĜA�A�\A1'A�A&�A%A��A�AA�A(�A��AA�AĜAZA  Al�A��AAp�A
=A�`A��A�uAC�A	��A	7LA��A��A�A"�Az�AZA$�A�A�A�A=qAƨA��A  AA�A|�AdZAC�A�!AZA�
A��AXA�A �/A �/A ��A ffA A�A 1'@�\)@��@��
@�l�@�l�@��@�E�@�G�@�A�@��@��@��@�V@�G�@� �@�@�ȴ@�^5@���@��@�h@�%@�bN@�F@���@��H@�@���@�t�@��@�R@柾@��@�-@�&�@䛦@���@�"�@�;d@�33@�o@��y@�!@��@�M�@�J@�-@�O�@���@�bN@�1'@��@��@�M�@ݲ-@���@ܬ@�z�@� �@ۍP@���@���@��@ش9@�(�@��m@�S�@��H@֧�@֏\@Ցh@ԃ@��@Ӯ@�"�@��@ҟ�@�-@��@�@�`B@�r�@���@Ϯ@�"�@��y@Χ�@Ͳ-@���@�z�@�r�@�Z@�1'@���@ˍP@�o@��@�hs@��`@�1'@��
@�l�@Ƨ�@�=q@��@ź^@�G�@�7L@�/@�&�@ě�@�Z@�ƨ@�\)@�
=@��H@��@§�@�v�@�J@�x�@�&�@�Z@��w@���@�C�@���@��@���@�O�@��@���@���@��u@�z�@�b@���@�K�@�+@�@���@��\@�@��@�G�@�O�@�G�@���@�Z@�1@��@�v�@�5?@��T@�`B@��@���@��u@�Z@�1'@��w@�@���@�~�@�n�@�n�@���@�~�@�=q@�$�@���@�G�@��`@��@���@�S�@�C�@�+@�o@��@�ȴ@���@��!@���@�ff@���@�?}@���@�1'@���@�C�@�@�ȴ@��@��@�5?@�-@��@�J@��@��h@�%@��`@���@�I�@��
@�33@�
=@��y@���@�V@�5?@�{@��@�`B@���@�z�@�b@��F@�t�@�C�@�+@��@��H@���@�=q@���@�?}@��@�&�@���@�Q�@��
@�ƨ@���@�+@�ȴ@�ȴ@�ȴ@���@�V@�-@���@��`@���@�j@�9X@�1@���@�33@�@���@�~�@��@�@���@�p�@�X@�O�@�hs@�7L@��#@��@��m@�t�@~@q�@i�#@_
=@U�h@HĜ@@�u@7|�@1&�@+��@&@!x�@j@ �@j@�u@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�hA�hA�hA�uA�uA╁A╁A◍A╁A╁A╁A╁A◍A♚A♚A◍A◍A╁A╁A◍A◍A◍A◍A◍A�hA�\A�\A�hA�\A�~�A�hsA�C�A�A�p�A߉7A�5?Aݰ!A��A��yAװ!AՃA��A���A��;AԓuA�`BA�  A�+A���A�VA��A�/A�-A�33A�oA���A�E�A̕�A˶FAʮA�z�A�^5AǴ9AǕ�A���A���A�dZA�9XA�XA�l�A��
A��HA�I�A��A�\)A��A���A�bNA�=qA�^5A��A�t�A��A���A�1A�z�A��hA���A�5?A��yA�=qA�ffA�XA�5?A���A��A�ȴA��A��A���A��DA�\)A�$�A��hA�ȴA�VA��FA�S�A�  A�(�Ayp�As��Al�\Ai��A^�`AZ�DAY;dAW&�AR�AM+AH��AG�TAGoAE��AD-AB��A??}A<ZA;C�A:�A8�HA7�#A6�A41A3�A3VA2�RA0�/A0ZA/�^A/
=A-��A,��A+�mA*�A(^5A'/A%ƨA%+A$��A$1A#�
A$�\A#��A#7LA"I�A �A�A(�A�A��A�
A��A�wA  Ar�A�An�AƨA33AĜA�A�\A1'A�A&�A%A��A�AA�A(�A��AA�AĜAZA  Al�A��AAp�A
=A�`A��A�uAC�A	��A	7LA��A��A�A"�Az�AZA$�A�A�A�A=qAƨA��A  AA�A|�AdZAC�A�!AZA�
A��AXA�A �/A �/A ��A ffA A�A 1'@�\)@��@��
@�l�@�l�@��@�E�@�G�@�A�@��@��@��@�V@�G�@� �@�@�ȴ@�^5@���@��@�h@�%@�bN@�F@���@��H@�@���@�t�@��@�R@柾@��@�-@�&�@䛦@���@�"�@�;d@�33@�o@��y@�!@��@�M�@�J@�-@�O�@���@�bN@�1'@��@��@�M�@ݲ-@���@ܬ@�z�@� �@ۍP@���@���@��@ش9@�(�@��m@�S�@��H@֧�@֏\@Ցh@ԃ@��@Ӯ@�"�@��@ҟ�@�-@��@�@�`B@�r�@���@Ϯ@�"�@��y@Χ�@Ͳ-@���@�z�@�r�@�Z@�1'@���@ˍP@�o@��@�hs@��`@�1'@��
@�l�@Ƨ�@�=q@��@ź^@�G�@�7L@�/@�&�@ě�@�Z@�ƨ@�\)@�
=@��H@��@§�@�v�@�J@�x�@�&�@�Z@��w@���@�C�@���@��@���@�O�@��@���@���@��u@�z�@�b@���@�K�@�+@�@���@��\@�@��@�G�@�O�@�G�@���@�Z@�1@��@�v�@�5?@��T@�`B@��@���@��u@�Z@�1'@��w@�@���@�~�@�n�@�n�@���@�~�@�=q@�$�@���@�G�@��`@��@���@�S�@�C�@�+@�o@��@�ȴ@���@��!@���@�ff@���@�?}@���@�1'@���@�C�@�@�ȴ@��@��@�5?@�-@��@�J@��@��h@�%@��`@���@�I�@��
@�33@�
=@��y@���@�V@�5?@�{@��@�`B@���@�z�@�b@��F@�t�@�C�@�+@��@��H@���@�=q@���@�?}@��@�&�@���@�Q�@��
@�ƨ@���@�+@�ȴ@�ȴ@�ȴ@���@�V@�-@���@��`@���@�j@�9X@�1@���@�33@�@���@�~�@��@�@���@�p�@�X@�O�@�hsG�O�@��#@��@��m@�t�@~@q�@i�#@_
=@U�h@HĜ@@�u@7|�@1&�@+��@&@!x�@j@ �@j@�u@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
33B
33B
33B
33B
33B
33B
2-B
2-B
1'B
/B
)�B
�B
uB
PB	��B	�)B	ɺB	ƨB	�B	�B
\B
�B
�B
33B
ZB
ÖB
�B
��B
��B
�NB
�)B
�yB
��B
�'B
�B
�dB
�B"�BD�B^5Be`Bu�B��B�7B��B��B��B�XB��B�=Bw�Bw�B}�B�B��B��B� B|�B{�BjB=qBbBVB
��B
�/B
��B
�hB
�=B
� B
y�B
q�B
l�B
gmB
cTB
]/B
O�B
K�B
� B
�bB
�PB
�+B
~�B
s�B
`BB
J�B
%�B	��B	��B	�XB	�VB	k�B	�B	B	  B��B�BB��B�B�B�B�B�B�B�5B�BB�5B�/B�#B�B�B�
B��B��B��B��B��B��B��B��B��B��B�#B�BB�`B�B�B��B	B	VB	%�B	'�B	%�B	&�B	%�B	$�B	'�B	)�B	,B	D�B	s�B	s�B	k�B	{�B	�B	}�B	z�B	u�B	y�B	x�B	� B	�%B	�1B	�JB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�!B	�?B	�jB	�jB	�dB	�wB	�qB	�jB	�jB	�dB	�dB	�jB	�qB	�jB	�^B	�jB	�jB	�^B	�FB	�9B	�^B	�jB	�jB	�qB	�qB	�qB	�^B	�FB	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�-B	�3B	�3B	�3B	�-B	�3B	�3B	�9B	�RB	�RB	�RB	�RB	�^B	�XB	�^B	�}B	��B	��B	ĜB	��B	��B	��B	��B	�
B	�B	�#B	�#B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�NB	�HB	�HB	�HB	�HB	�BB	�BB	�BB	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�BB	�;B	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
  B
  B	��B	��B
  B
B
B
B
1B
+B
1B
1B
1B
1B
+B
+B
+B
+B
%B
B
B
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
1B
1B
	7B
1B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B
	7B
	7B
DB
DB
JB
JB
JB
JB
JB
PB
PB
VB
VB
VB
VB
VB
VB
VB
\B
uB
\B
�B
!�B
'�B
2-B
<jB
@�B
E�B
H�B
P�B
VB
\)B
`BB
e`B
hsB
l�B
p�B
t�B
w�B
z�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
4B
4B
4B
4B
4B
4B
4B
4B
4B
4B
4B
4B
4B
4B
4B
4B
4B
4B
4B
4B
4B
4B
4B
3
B
3	B
3B
3
B
3
B
3
B
2B
2B
0�B
.�B
)�B
~B
KB
(B	��B	��B	ɐB	�}B	��B	�gB
2B
oB
�B
3B
Y�B
�mB
�UB
��B
��B
�#B
� B
�PB
��B
��B
��B
�9B
�~B"�BDqB^
Be4Bu�B�TB�B�oB��B��B�-B�VB�Bw�Bw�B}�B��B�wB�qB�B|�B{�BjSB=?B0B'B
��B
�B
��B
�5B
�B
�B
y�B
qwB
lWB
g8B
c#B
\�B
O�B
K�B
�B
�/B
�B
��B
~�B
s�B
`B
J�B
%�B	��B	��B	�!B	�B	kLB	sB	�B��B��B�	BӼB��B��B��B��B��B��B��B�B��B��B��B��B��B��B��BӽBѯBҴBҶBѯBЩBѮBҴB��B��B�B�!B�OB�QB��B	�B	B	%�B	'�B	%�B	&�B	%�B	$�B	'�B	)�B	+�B	D^B	sxB	sxB	kFB	{�B	��B	}�B	z�B	u�B	y�B	x�B	�B	��B	��B	�B	�#B	�3B	�=B	�GB	�tB	�mB	�bB	�OB	�NB	�QB	�OB	�LB	�QB	�TB	�ZB	�`B	�`B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�(B	�,B	�#B	�5B	�1B	�*B	�)B	�"B	�$B	�)B	�/B	�)B	�B	�(B	�)B	�B	�B	��B	�B	�)B	�)B	�0B	�1B	�2B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�;B	�AB	�=B	�]B	˃B	͒B	ϝB	ѬB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�*B	�*B	�+B	�+B	�0B	�4B	�;B	�;B	�;B	�BB	�:B	�CB	�AB	�MB	�MB	�MB	�OB	�MB	�OB	�IB	�GB	�HB	�FB	�GB	�IB	�BB	�?B	�GB	�@B	�AB	�@B	�AB	�FB	�[B	�\B	�]B	�_B	�cB	�jB	�pB	�qB	�kB	�mB	�lB	�oB	�qB	�pB	�pB	�rB	�tB	�qB	�rB	�rB	�xB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
 �B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
 B
 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
�B
�B

�B

�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
G�O�B
B
rB
!�B
'�B
1�B
<$B
@?B
E[B
HmB
P�B
U�B
[�B
_�B
eB
h.B
lFB
p^B
txB
w�B
z�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.53 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007552019040510075520190405100755  AO  ARCAADJP                                                                    20181121125947    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125947  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125947  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100755  IP                  G�O�G�O�G�O�                