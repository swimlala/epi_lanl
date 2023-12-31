CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-01-30T08:58:00Z creation; 2021-03-26T17:01:01Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  d|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � Al   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20210130085800  20210326170212  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               @   @AA  AOAO7836_008777_064                 7836_008777_064                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @�Z����+@�Z����+11  @�Z��c�@�Z��c�@;�o��m@;�o��m�d��İ��d��İ�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?��H@@  @��\@�G�@��R@�  A   A\)A   A,��A@  A_\)A\)A�Q�A�Q�A�Q�A�Q�A�  A��A��B   B�
B�
B�
B   B(  B/�
B7�
B@(�BH(�BO�
BW�B_�Bg�
Bp  Bx  B�
B�{B�{B�{B�  B��B�  B��B�  B�  B�B��
B��B��B�  B��B�{B�(�B�{B��B��B�  B��
B�  B�{B��
B��
B�  B��B��
B�{B�(�C 
=C
=C  C  C
=C
{C
=C�C�C  C  C
=C��C��C  C��C�C!��C$  C&
=C(  C*
=C,
=C.  C0
=C2{C3��C5�C7�C:  C<  C=�C?��CA�CD  CE��CG��CJ
=CK��CM�HCO�CQ��CT
=CV  CX
=CY��C[��C^  C`{Cb
=Cd  Cf  Cg��Ci��Ck��Cm�Co��Cr  Cs��Cu��Cx  Cz{C|{C~  C�  C�  C�  C�C�
=C�
=C�  C���C���C���C�
=C���C���C�C�  C�C�C�C�  C���C�C�  C�  C�  C�  C�  C�C�
=C�C�
=C���C�  C���C���C�  C���C�C�
=C�\C�\C�
=C�  C�C�
=C�  C���C�  C�  C�  C�C�  C���C�  C�C�C�  C�  C���C���C�  C�C�C�\C�  C���C�  C�C�C���C���C���C�  C�C�  C�C�C�  C�\C�C���C���C�  C�
=C���C���C���C�C�  C���C�  C�  C�  C�C�C���C��C���C�  C�  C�C�  C���C���C�  C�  C���C���C�  C�C�C�  C�C�C�  C�  C���C���C�  C���C�C�C�  C���C���C�
=C�  C���C�D �D }qD �qD}qD�D��D�qDz�D�RD}qD��Dz�D�qD� D�D��D  D��D	  D	� D
�D
��D�D�D  D� D�D� D  D� D  D�DD� D�D�D  D}qD�qD� D�qD}qD�qDz�D��D� D�D��D  D}qD�qD}qD�D�D  Dz�D�qD��D  D}qD��D� D�D� D �D ��D!  D!}qD!�qD"� D#�D#}qD$  D$��D%  D%� D&�D&��D'  D'��D'�qD(� D)�D)� D*  D*��D+  D+� D,  D,�D-�D-}qD.  D.� D/  D/��D/�qD0� D1  D1}qD2  D2}qD2��D3}qD4  D4� D5  D5��D6�D6��D7�D7��D7�qD8� D9�D9��D9�qD:}qD;�D;�D<D<}qD<�qD=� D>  D>}qD?  D?��D@�D@��DA  DA� DB  DB� DC  DC��DC�qDD}qDE  DE�DF  DF� DF�qDGz�DH�DH��DI�DI� DI�qDJz�DJ�qDK}qDL  DL� DM  DM��DN�DN� DN�qDO� DP�DP� DQ�DQ��DR�DR}qDR��DS� DT�DT� DT�qDU� DV  DV}qDW  DW� DX�DX��DX�qDY}qDZ  DZ��D[�D[��D\  D\� D]�D]�D^  D^}qD^�qD_}qD_�qD`� D`�qDa� Db  Db� Dc  Dc}qDd�Dd��De  De� DfDf��Dg  Dgz�Dg�qDh� Di�Di��Di�qDj� Dk  Dk��Dl�Dl� Dl�qDm� Dn  Dn}qDo  Do� Do�qDp}qDq�Dq� Dr  Dr��Ds  Ds}qDs�qDt}qDu  Du��Dv  Dv}qDw  Dw}qDw�qDx� Dy  Dy� Dy�qDz}qDz�qD{}qD|�D|��D}�D}�D~�D~��D~�qD� D�HD�@ D�� D��HD�  D�@ D���D�� D��qD�=qD�}qD�� D�HD�AHD��HD�� D�  D�B�D���D��HD�HD�C�D���D��HD���D�=qD�}qD���D�  D�@ D�~�D�� D�  D�@ D�� D���D�  D�AHD��HD���D���D�@ D�� D�� D�HD�AHD��HD�� D�HD�@ D�~�D��HD�HD�AHD�� D���D���D�>�D�~�D�� D���D�>�D�~�D�� D�  D�@ D�� D�� D�HD�@ D�~�D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�  D�AHD��HD���D�  D�B�D�� D���D���D�@ D��HD�� D���D�>�D�� D�� D�  D�B�D�� D�� D�  D�>�D�� D���D�  D�AHD���D�� D��qD�>�D�� D�� D�  D�@ D�� D��HD�HD�>�D�~�D�� D�HD�AHD�~�D�� D��D�B�D��HD��HD�  D�AHD��HD��HD�  D�>�D�~�D�� D�HD�>�D�� D���D���D�@ D��HD�D�  D�>�D�~�D���D�  D�@ D�� D�� D���D�=qD�� D�D�  D�=qD�� D���D���D�@ D�� D��HD�  D�AHD�� D���D��qD�=qD�~�D�� D�HD�AHD��HD��HD�  D�=qD�� D�� D���D�>�D��HD��HD���D�=qD�~�D��HD�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�AHD���D��HD�  D�>�D�}qD��qD�  D�@ D�~�D�� D���D�=qD�~�D��HD��D�AHD�� D���D�  D�AHD��HD�� D�  D�AHD�� D�� D�HD�>�D�~�D�� D���D�>�D�}qD��qD��qD�=qD�� D��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�HD�AHD��HD�� D�  D�@ D�� D���D�  D�@ D�� D��HD�HD�=qD�~�D���D���D�@ DHD��HD���D�>�DÀ D�D�HD�>�D�~�D�� D���D�@ Dŀ DŽqD��qD�=qD�~�D�� D�  D�@ D�}qDǾ�D�HD�>�D�~�D��HD�HD�AHDɀ DɽqD�  D�AHDʀ Dʾ�D��qD�@ D˂�D�� D���D�@ D̀ D��HD�  D�@ D�~�D;�D���D�>�D�~�Dξ�D���D�@ DρHD��HD�  D�@ DЁHDо�D�HD�B�DсHDѾ�D���D�AHD�~�DҾ�D�HD�AHDӁHD�D�HD�>�DԀ D�� D�  D�@ DՀ D��HD�HD�@ D�~�D�� D�  D�AHDׁHD�� D�  D�AHD؀ Dؾ�D�HD�B�Dق�D�D�HD�@ Dڀ D��HD��D�AHD�~�D۾�D���D�@ D܀ Dܾ�D���D�>�D݀ Dݾ�D�  D�AHDހ D�� D�HD�AHD߁HD��HD���D�=qD�~�DྸD�  D�@ D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�HD��HD���D�@ D�HD��HD�HD�B�D傏D�� D�  D�AHD� D�� D�  D�@ D� D羸D�  D�AHD� D��HD���D�=qD� D�� D���D�>�D� D�� D�HD�@ D�HD��HD�HD�=qD�}qD쾸D�HD�@ D� D��HD��D�AHD� D��HD�HD�@ D�~�DﾸD���D�>�D��HD��HD�  D�@ D�D���D�HD�@ D� D��HD��D�@ D� D��HD�  D�@ D�HD��HD�HD�@ D�~�D���D�HD�AHD�� D�� D�  D�AHD��HD��HD���D�=qD�}qD���D���D�@ D�� D�� D�HD�AHD��HD��HD���?\)?��?L��?�\)?�{?�G�?��H@
=q@��@(��@8Q�@E�@Y��@p��@��\@���@�\)@�Q�@��R@�ff@���@���@�Q�@��
@˅@�z�@�p�@�ff@���@�z�@�(�A�A
=A�A  A�
AQ�A(�A   A#�
A'
=A+�A0  A5�A:=qA=p�AAG�AE�AHQ�AL(�AP  AU�AY��A^�RAb�\AfffAh��Al��Ap  Atz�Ay��A~{A�G�A�33A�z�A�ffA���A��A��
A�ffA���A��HA�p�A�\)A�=qA�(�A�A��A���A�(�A�{A�  A�=qA�z�A�
=A���A��A�A�
=A���A��HA�(�A�{A�Q�A��A�z�AƸRA�Q�Aʏ\A���AθRAУ�A�=qA�(�A�{A�  Aٙ�AۅA�{A�Q�A��HA��A�A�=qA�(�A�ffA�Q�A��HA��A�\)A���A��
A�ffB Q�B��B�RB  B�B{B
=B  B	G�B
{B�BQ�Bp�BffB�B��B{B33Bz�B��B�RB�
B��B{B
=B(�B�B=qB\)B z�B!��B"�\B#�B$��B%B&�HB(  B)G�B*=qB+\)B,Q�B-p�B.�\B/�
B1�B2{B3\)B4��B5B6�HB8Q�B9G�B:ffB;\)B<��B=��B>�HB@  BA�BB{BC\)BDz�BE��BF�RBG�
BH��BJ{BK
=BL(�BMG�BNffBO�BP��BQBR�HBT(�BUG�BVffBW�BX��BZ{B[
=B\(�B]p�B^�RB_�
B`��Bb=qBc�Bd��BeBf�HBh  Bi�Bj=qBk33Blz�BmBn�HBp(�Bqp�Br�\Bs�Bt��Bv{Bw33BxQ�By��Bz�HB|  B}G�B~ffB�
B�z�B�
=B���B�(�B��RB�G�B��B�z�B��B�B�Q�B���B��B�(�B��RB�G�B��B�z�B��B���B�=qB��RB�\)B��
B��\B��B�B�ffB�
=B���B�=qB��HB���B�=qB���B�\)B�  B�z�B�
=B��B�(�B��HB��B�(�B���B�p�B�{B���B��B�B�Q�B���B�p�B�  B���B�33B��B�z�B��B��
B�ffB�
=B���B�(�B���B�33B�B�Q�B���B�p�B�  B���B�G�B��B��\B�33B�B�=qB���B�\)B��B�z�B���B��B�=qB���B���B�=qB���B��B�B�z�B��B��
B�Q�B��HB�\)B�  B���B�p�B��B�ffB���B��B�Q�B�
=B�B�Q�B��HB�p�B��
B£�B�p�B�  B�z�B��HBř�B�Q�B�
=BǅB�  Bȏ\B�\)B�  BʸRB�G�B��
B�Q�B���BͅB�(�B��HBϮB�(�BиRB�33B�Bҏ\B�33B��Bԏ\B�
=BՅB�Q�B�
=B׮B�=qBأ�B�\)B�  Bڣ�B�p�B��B�Q�B���B�B�ffB��B߮B�(�B���B�G�B�  B�RB�\)B�B�ffB�33B噚B�{B���B癚B�{B�\B��B��
B�\B�33B�B�(�B��HB홚B�Q�B��HB�p�B��B��B�p�B�{B�\B�
=B��
B�\B��B��B�Q�B��B���B�{B��HB��B��B���B�\)B��B���B�33B��B���B�
=B�C =qC p�C �HC33CffC��C(�C\)C��C
=C\)CC�C\)C�C�CQ�C��C�CQ�C�C�HCG�C�C��C33C�\C��C	=qC	z�C	C
33C
ffC
��C�C\)C��C�CffC�
C
=Cp�C��C
=C�CC{Cz�C�C�CffCC�C\)C��C
=Cp�C��C
=Cz�CC{Cp�C�RC(�Cz�C�RC33CffC�
C{Cp�C��C{C�\CC�Cz�C�RC�Cz�C�RC�Cp�C�C(�CffCC�CffC�
C{Cz�C��C(�C�\C��C (�C �\C ��C!=qC!�\C!��C"G�C"�C"�C#=qC#�\C#��C$33C$�C$�HC%Q�C%��C%�C&Q�C&�\C'  C'33C'��C'�C(=qC(��C(�HC)Q�C)�C*  C*=qC*�C*��C+\)C+�C,  C,p�C,�C-(�C-ffC-�
C.(�C.�C.�C/=qC/��C/��C0Q�C0�RC1  C1z�C1�RC233C2�C2�HC3=qC3�\C4  C4=qC4C5
=C5\)C5��C6{C6�C6C7=qC7�\C7��C8Q�C8��C9{C9Q�C9��C:
=C:�C:��C;33C;�\C;�HC<\)C<�\C=
=C=\)C=�RC>(�C>p�C>�
C?=qC?z�C?��C@Q�C@��CA{CA\)CACB�CBffCB�HCC(�CC�\CD  CD33CD�CD��CEG�CE��CF
=CF\)CF��CG33CGp�CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 ?u?��H@@  @��\@�G�@��R@�  A   A\)A   A,��A@  A_\)A\)A�Q�A�Q�A�Q�A�Q�A�  A��A��B   B�
B�
B�
B   B(  B/�
B7�
B@(�BH(�BO�
BW�B_�Bg�
Bp  Bx  B�
B�{B�{B�{B�  B��B�  B��B�  B�  B�B��
B��B��B�  B��B�{B�(�B�{B��B��B�  B��
B�  B�{B��
B��
B�  B��B��
B�{B�(�C 
=C
=C  C  C
=C
{C
=C�C�C  C  C
=C��C��C  C��C�C!��C$  C&
=C(  C*
=C,
=C.  C0
=C2{C3��C5�C7�C:  C<  C=�C?��CA�CD  CE��CG��CJ
=CK��CM�HCO�CQ��CT
=CV  CX
=CY��C[��C^  C`{Cb
=Cd  Cf  Cg��Ci��Ck��Cm�Co��Cr  Cs��Cu��Cx  Cz{C|{C~  C�  C�  C�  C�C�
=C�
=C�  C���C���C���C�
=C���C���C�C�  C�C�C�C�  C���C�C�  C�  C�  C�  C�  C�C�
=C�C�
=C���C�  C���C���C�  C���C�C�
=C�\C�\C�
=C�  C�C�
=C�  C���C�  C�  C�  C�C�  C���C�  C�C�C�  C�  C���C���C�  C�C�C�\C�  C���C�  C�C�C���C���C���C�  C�C�  C�C�C�  C�\C�C���C���C�  C�
=C���C���C���C�C�  C���C�  C�  C�  C�C�C���C��C���C�  C�  C�C�  C���C���C�  C�  C���C���C�  C�C�C�  C�C�C�  C�  C���C���C�  C���C�C�C�  C���C���C�
=C�  C���C�D �D }qD �qD}qD�D��D�qDz�D�RD}qD��Dz�D�qD� D�D��D  D��D	  D	� D
�D
��D�D�D  D� D�D� D  D� D  D�DD� D�D�D  D}qD�qD� D�qD}qD�qDz�D��D� D�D��D  D}qD�qD}qD�D�D  Dz�D�qD��D  D}qD��D� D�D� D �D ��D!  D!}qD!�qD"� D#�D#}qD$  D$��D%  D%� D&�D&��D'  D'��D'�qD(� D)�D)� D*  D*��D+  D+� D,  D,�D-�D-}qD.  D.� D/  D/��D/�qD0� D1  D1}qD2  D2}qD2��D3}qD4  D4� D5  D5��D6�D6��D7�D7��D7�qD8� D9�D9��D9�qD:}qD;�D;�D<D<}qD<�qD=� D>  D>}qD?  D?��D@�D@��DA  DA� DB  DB� DC  DC��DC�qDD}qDE  DE�DF  DF� DF�qDGz�DH�DH��DI�DI� DI�qDJz�DJ�qDK}qDL  DL� DM  DM��DN�DN� DN�qDO� DP�DP� DQ�DQ��DR�DR}qDR��DS� DT�DT� DT�qDU� DV  DV}qDW  DW� DX�DX��DX�qDY}qDZ  DZ��D[�D[��D\  D\� D]�D]�D^  D^}qD^�qD_}qD_�qD`� D`�qDa� Db  Db� Dc  Dc}qDd�Dd��De  De� DfDf��Dg  Dgz�Dg�qDh� Di�Di��Di�qDj� Dk  Dk��Dl�Dl� Dl�qDm� Dn  Dn}qDo  Do� Do�qDp}qDq�Dq� Dr  Dr��Ds  Ds}qDs�qDt}qDu  Du��Dv  Dv}qDw  Dw}qDw�qDx� Dy  Dy� Dy�qDz}qDz�qD{}qD|�D|��D}�D}�D~�D~��D~�qD� D�HD�@ D�� D��HD�  D�@ D���D�� D��qD�=qD�}qD�� D�HD�AHD��HD�� D�  D�B�D���D��HD�HD�C�D���D��HD���D�=qD�}qD���D�  D�@ D�~�D�� D�  D�@ D�� D���D�  D�AHD��HD���D���D�@ D�� D�� D�HD�AHD��HD�� D�HD�@ D�~�D��HD�HD�AHD�� D���D���D�>�D�~�D�� D���D�>�D�~�D�� D�  D�@ D�� D�� D�HD�@ D�~�D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�  D�AHD��HD���D�  D�B�D�� D���D���D�@ D��HD�� D���D�>�D�� D�� D�  D�B�D�� D�� D�  D�>�D�� D���D�  D�AHD���D�� D��qD�>�D�� D�� D�  D�@ D�� D��HD�HD�>�D�~�D�� D�HD�AHD�~�D�� D��D�B�D��HD��HD�  D�AHD��HD��HD�  D�>�D�~�D�� D�HD�>�D�� D���D���D�@ D��HD�D�  D�>�D�~�D���D�  D�@ D�� D�� D���D�=qD�� D�D�  D�=qD�� D���D���D�@ D�� D��HD�  D�AHD�� D���D��qD�=qD�~�D�� D�HD�AHD��HD��HD�  D�=qD�� D�� D���D�>�D��HD��HD���D�=qD�~�D��HD�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�AHD���D��HD�  D�>�D�}qD��qD�  D�@ D�~�D�� D���D�=qD�~�D��HD��D�AHD�� D���D�  D�AHD��HD�� D�  D�AHD�� D�� D�HD�>�D�~�D�� D���D�>�D�}qD��qD��qD�=qD�� D��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�HD�AHD��HD�� D�  D�@ D�� D���D�  D�@ D�� D��HD�HD�=qD�~�D���D���D�@ DHD��HD���D�>�DÀ D�D�HD�>�D�~�D�� D���D�@ Dŀ DŽqD��qD�=qD�~�D�� D�  D�@ D�}qDǾ�D�HD�>�D�~�D��HD�HD�AHDɀ DɽqD�  D�AHDʀ Dʾ�D��qD�@ D˂�D�� D���D�@ D̀ D��HD�  D�@ D�~�D;�D���D�>�D�~�Dξ�D���D�@ DρHD��HD�  D�@ DЁHDо�D�HD�B�DсHDѾ�D���D�AHD�~�DҾ�D�HD�AHDӁHD�D�HD�>�DԀ D�� D�  D�@ DՀ D��HD�HD�@ D�~�D�� D�  D�AHDׁHD�� D�  D�AHD؀ Dؾ�D�HD�B�Dق�D�D�HD�@ Dڀ D��HD��D�AHD�~�D۾�D���D�@ D܀ Dܾ�D���D�>�D݀ Dݾ�D�  D�AHDހ D�� D�HD�AHD߁HD��HD���D�=qD�~�DྸD�  D�@ D�~�DᾸD���D�>�D�~�D⾸D���D�>�D�HD��HD���D�@ D�HD��HD�HD�B�D傏D�� D�  D�AHD� D�� D�  D�@ D� D羸D�  D�AHD� D��HD���D�=qD� D�� D���D�>�D� D�� D�HD�@ D�HD��HD�HD�=qD�}qD쾸D�HD�@ D� D��HD��D�AHD� D��HD�HD�@ D�~�DﾸD���D�>�D��HD��HD�  D�@ D�D���D�HD�@ D� D��HD��D�@ D� D��HD�  D�@ D�HD��HD�HD�@ D�~�D���D�HD�AHD�� D�� D�  D�AHD��HD��HD���D�=qD�}qD���D���D�@ D�� D�� D�HD�AHD��HD��HG�O�?\)?��?L��?�\)?�{?�G�?��H@
=q@��@(��@8Q�@E�@Y��@p��@��\@���@�\)@�Q�@��R@�ff@���@���@�Q�@��
@˅@�z�@�p�@�ff@���@�z�@�(�A�A
=A�A  A�
AQ�A(�A   A#�
A'
=A+�A0  A5�A:=qA=p�AAG�AE�AHQ�AL(�AP  AU�AY��A^�RAb�\AfffAh��Al��Ap  Atz�Ay��A~{A�G�A�33A�z�A�ffA���A��A��
A�ffA���A��HA�p�A�\)A�=qA�(�A�A��A���A�(�A�{A�  A�=qA�z�A�
=A���A��A�A�
=A���A��HA�(�A�{A�Q�A��A�z�AƸRA�Q�Aʏ\A���AθRAУ�A�=qA�(�A�{A�  Aٙ�AۅA�{A�Q�A��HA��A�A�=qA�(�A�ffA�Q�A��HA��A�\)A���A��
A�ffB Q�B��B�RB  B�B{B
=B  B	G�B
{B�BQ�Bp�BffB�B��B{B33Bz�B��B�RB�
B��B{B
=B(�B�B=qB\)B z�B!��B"�\B#�B$��B%B&�HB(  B)G�B*=qB+\)B,Q�B-p�B.�\B/�
B1�B2{B3\)B4��B5B6�HB8Q�B9G�B:ffB;\)B<��B=��B>�HB@  BA�BB{BC\)BDz�BE��BF�RBG�
BH��BJ{BK
=BL(�BMG�BNffBO�BP��BQBR�HBT(�BUG�BVffBW�BX��BZ{B[
=B\(�B]p�B^�RB_�
B`��Bb=qBc�Bd��BeBf�HBh  Bi�Bj=qBk33Blz�BmBn�HBp(�Bqp�Br�\Bs�Bt��Bv{Bw33BxQ�By��Bz�HB|  B}G�B~ffB�
B�z�B�
=B���B�(�B��RB�G�B��B�z�B��B�B�Q�B���B��B�(�B��RB�G�B��B�z�B��B���B�=qB��RB�\)B��
B��\B��B�B�ffB�
=B���B�=qB��HB���B�=qB���B�\)B�  B�z�B�
=B��B�(�B��HB��B�(�B���B�p�B�{B���B��B�B�Q�B���B�p�B�  B���B�33B��B�z�B��B��
B�ffB�
=B���B�(�B���B�33B�B�Q�B���B�p�B�  B���B�G�B��B��\B�33B�B�=qB���B�\)B��B�z�B���B��B�=qB���B���B�=qB���B��B�B�z�B��B��
B�Q�B��HB�\)B�  B���B�p�B��B�ffB���B��B�Q�B�
=B�B�Q�B��HB�p�B��
B£�B�p�B�  B�z�B��HBř�B�Q�B�
=BǅB�  Bȏ\B�\)B�  BʸRB�G�B��
B�Q�B���BͅB�(�B��HBϮB�(�BиRB�33B�Bҏ\B�33B��Bԏ\B�
=BՅB�Q�B�
=B׮B�=qBأ�B�\)B�  Bڣ�B�p�B��B�Q�B���B�B�ffB��B߮B�(�B���B�G�B�  B�RB�\)B�B�ffB�33B噚B�{B���B癚B�{B�\B��B��
B�\B�33B�B�(�B��HB홚B�Q�B��HB�p�B��B��B�p�B�{B�\B�
=B��
B�\B��B��B�Q�B��B���B�{B��HB��B��B���B�\)B��B���B�33B��B���B�
=B�C =qC p�C �HC33CffC��C(�C\)C��C
=C\)CC�C\)C�C�CQ�C��C�CQ�C�C�HCG�C�C��C33C�\C��C	=qC	z�C	C
33C
ffC
��C�C\)C��C�CffC�
C
=Cp�C��C
=C�CC{Cz�C�C�CffCC�C\)C��C
=Cp�C��C
=Cz�CC{Cp�C�RC(�Cz�C�RC33CffC�
C{Cp�C��C{C�\CC�Cz�C�RC�Cz�C�RC�Cp�C�C(�CffCC�CffC�
C{Cz�C��C(�C�\C��C (�C �\C ��C!=qC!�\C!��C"G�C"�C"�C#=qC#�\C#��C$33C$�C$�HC%Q�C%��C%�C&Q�C&�\C'  C'33C'��C'�C(=qC(��C(�HC)Q�C)�C*  C*=qC*�C*��C+\)C+�C,  C,p�C,�C-(�C-ffC-�
C.(�C.�C.�C/=qC/��C/��C0Q�C0�RC1  C1z�C1�RC233C2�C2�HC3=qC3�\C4  C4=qC4C5
=C5\)C5��C6{C6�C6C7=qC7�\C7��C8Q�C8��C9{C9Q�C9��C:
=C:�C:��C;33C;�\C;�HC<\)C<�\C=
=C=\)C=�RC>(�C>p�C>�
C?=qC?z�C?��C@Q�C@��CA{CA\)CACB�CBffCB�HCC(�CC�\CD  CD33CD�CD��CEG�CE��CF
=CF\)CF��CG33CGp�CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�V@�@��@ȞG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�M�A�M�A�M�A�M�A�O�A�Q�A�Q�A�Q�A�Q�A�O�A�M�A�M�A�S�A�ZA�ZA�XA�ZA�XA��jA�v�A�jA�1A��A��yA��#A���A���A�n�A���A���A�bNA�JA��mA�~�A�O�A�9XA�+A�"�A�JA�A��DA�p�A�XA�7LA��A��wA��7A�~�A�C�A��DA���A�$�A��uA�K�A��
A�G�A�=qA��HA��wA���A�\)A�VA�ĜA��hA�~�A�ȴA��A�hsA�p�A�O�A��/A���A���A�(�A��-A�l�A��A�K�A��7A�bNA��A�Q�A�5?A��
A���A�|�A�VA���A�  A�Q�A���A�G�A��9A�I�A��jA�t�A�VA��RA�z�A�/A33A~�A|�HA{l�AzVAy�Ax$�At�yAs�TAsx�Ar��AqK�Ap�yAo�TAo�AnjAm��Al��Ak�AkS�Aj�Ai��AhA�Ag�
Af�9Ae��Ae%Adr�Ac�AbE�A`{A^�A\��A\�\A\�A[\)AZ �AX�DAW�AWdZAV~�AR9XAP �AO?}AN�AMƨAKAI�AGp�AFbAE�TAE�#AE��AE�^AD�AB(�A>�RA=A<�/A<1A;�TA;��A;��A;`BA;oA:ffA9�hA9+A8��A8VA7��A7+A6�uA6A5ƨA5t�A4I�A3
=A2ffA0-A-�7A+�TA+�A*1A)t�A(�A(�jA(��A(^5A'��A&r�A%XA$��A$jA$1A"�uA"=qA!�-A jA�;A��A1A�PAz�AdZAC�A33A�A��A��A��A�uA-A�^A/A��AVA1'A�At�A/A�9A&�A~�At�A-A�7A��A�DA�A;dA7LA
��A
^5A
bA	��A�`At�At�A��A+A��A n�A E�@���@�E�@�x�@��w@�
=@�-@��-@�dZ@���@��7@�z�@�|�@�\@�@�hs@��/@�33@�!@�=q@�bN@�$�@��;@�l�@��@�n�@�@���@��@�ff@���@ݺ^@�x�@�z�@���@�Ĝ@��
@�l�@�^5@�?}@�z�@���@�@ёh@�Q�@ΰ!@���@�p�@�Ĝ@� �@˝�@�"�@ʗ�@�n�@��@���@�X@���@�Z@š�@��/@�r�@��
@�l�@��y@\@��-@��@���@�?}@��u@�1@��@�"�@�-@�G�@���@�J@�`B@��@��D@��@�"�@�ff@���@��j@��F@���@�%@���@�33@���@�5?@��@�V@��j@�I�@��@�~�@��^@�j@�ƨ@��R@�=q@���@�x�@�%@���@�1'@��F@�\)@�V@�@���@�O�@���@�1'@�b@�|�@�o@���@��@��^@�x�@�/@��/@�Ĝ@���@�33@��@��@���@���@���@�^5@��@��@��#@�p�@���@�z�@�ƨ@�;d@��@��!@���@�ff@�@���@���@�x�@�7L@��/@��9@��u@�j@�I�@�(�@�dZ@�E�@��@���@��@�`B@�&�@��/@���@��j@�Q�@���@��F@�t�@�C�@���@��R@��+@�ff@�E�@�$�@�@��@��@��/@�bN@�1'@�b@��@�S�@�"�@�@��y@�^5@���@�`B@�9X@��@��R@���@���@�^5@���@�G�@�V@��@�j@�@}�-@}�@}`B@}V@|�/@|�j@|�j@|�j@|��@|z�@|j@|9X@{�m@{t�@{S�@{C�@{"�@z��@z~�@z-@zJ@y��@y��@y�#@y��@y�^@y��@y��@yhs@yX@yX@y�@x��@x�@x  @v�@v@u/@t�@t��@s�m@sS�@s33@so@r��@r^5@rM�@r-@rJ@q�#@q�7@q&�@p�`@p�@pr�@pA�@pA�@pbN@pr�@pA�@o�w@n��@nV@m@l�j@k�F@k�@kS�@kC�@k33@ko@j�!@j�@ihs@i�@hbN@g��@f�y@f�+@f@ep�@e�@eV@d�@d�@dZ@d(�@c�F@cS�@ct�@cS�@co@b�@bM�@a�@a��@a�7@`bN@_��@_�w@_\)@^�y@^��@^��@^5?@]��@\��@\�j@\��@\�D@\I�@[�m@[��@[�F@[S�@[33@[@Z�@[@Y��@Y�^@Y��@Y7L@Xr�@Xr�@X1'@W�w@W�P@W;d@W+@W�@Vȴ@Vff@V5?@U�@U/@UV@TZ@T�@T1@S�
@SdZ@So@So@So@So@S@R��@R�\@R�\@R~�@R-@Q�#@Q��@Q7L@Q&�@Q�@P��@PQ�@O|�@OK�@O�@Nȴ@N��@N��@N5?@M�@M?}@L�@L�@L�D@Lz�@LI�@L�@K�F@K33@J�@J��@J^5@JJ@I�^@I��@I7L@H��@H��@Hr�@H1'@G�@G;d@Fv�@E�@E�h@EO�@Dz�@C�m@Ct�@Bn�@A�@A�^@Ax�@A&�@A�@A%@@��@@Ĝ@@A�@?�;@?�;@?�w@>��@>��@>�+@>$�@=�@=O�@=/@=V@<�D@;�
@;��@;o@:�\@:J@9��@9�^@9��@9�7@9X@9G�@97L@9�@9%@8�`@8��@8Ĝ@8��@8�u@8�@8b@7��@7��@7�P@7|�@7\)@7;d@7+@6�y@6ff@5�-@5`B@5�@4��@4�@4�/@4�j@4I�@3��@3��@3�
@3�
@3�
@3�@2�@2�\@2n�@2=q@1��@1�#@1�^@1��@1&�@0��@0�`@0�`@0�9@0bN@0b@/�@/�;@/��@/�w@/��@/�P@/l�@/;d@.��@.ȴ@.��@.E�@.{@.@-�T@-�T@-�T@-��@,�@,�j@,�@,�@,j@,Z@,9X@+��@+dZ@+dZ@+S�@+S�@+S�@+S�@+C�@+33@+"�@+o@+@*�@*�@*�H@*��@*~�@*^5@*�@)��@)7L@(�`@(��@(r�@( �@'��@';d@';d@'+@'�@'
=@&��@&�y@&�y@&�@&��@&E�@&5?@&@%@%�-@%�-@%�h@%?}@$�@$�/@$��@$��@$Z@$I�@$(�@#�m@#��@#C�@"�@"��@"��@"��@"�\@"M�@"�@!�#@!�^@!��@!��@!x�@ ��@ Ĝ@ ��@ �@ bN@ b@
=@��@��@�+@v�@v�@V@$�@�@��@@��@`B@�@�/@�j@�@(�@�@1@�m@ƨ@�F@��@dZ@"�@@�\@n�@n�@=q@��@�#@��@�^@�^@x�@&�@�`@Ĝ@Q�@�;@�P@|�@l�@\)@K�@;d@;d@��@�@�R@��@v�@$�@�@��@�-@��@�h@�@?}@�@�@�/@�j@�@z�@�@��@t�@dZ@dZ@C�@33@@��@^5@=q@=q@-@J@�#@��@x�@7L@��@r�@1'@1'@b@�;@��@l�@�@��@�@��@�+@v�@ff@5?@@��@�-@`B@?}@?}@/@�@��@�/@�@Z@�@�F@dZ@"�@
�!@
�\@
n�@
-@	�^@	��@	X@	&�@��@�9@�u@bN@bN@r�@Q�@  @�;@�@��@l�@�@��@�y@�@�@�@ȴ@�R@V@{@�@?}@�@�j@��@j@�@��@ƨ@��@t�@C�@"�@o@o@�H@�!@~�@M�@=q@�@�@��@��@��@��@�7@hs@G�@7L@&�@�@�@%@%@ �`@ ��@ Ĝ@ �9A�K�A�M�A�K�A�M�A�M�A�M�A�Q�A�O�A�O�A�M�A�M�A�M�A�M�A�I�A�K�A�M�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�O�A�Q�A�Q�A�S�A�S�A�S�A�Q�A�O�A�O�A�S�A�Q�A�Q�A�Q�A�S�A�Q�A�O�A�K�A�I�A�I�A�M�A�O�A�O�A�M�A�M�A�K�A�K�A�K�A�K�A�O�A�VA�VA�XA�XA�XA�XA�XA�ZA�ZA�ZA�XA�ZA�\)A�\)A�XA�XA�XA�XA�XA�VA�ZA�ZA�\)A�ZA�XA�XA�XA�XA�VA�VA�XA�ZA�ZA�\)A�^5A�^5A�`BA�^5A�\)A�\)A�ZA�\)A�ZA�C�A�+A��A��A���A�dZA�hsA�ffA�l�A�A�/A�7LA�K�A�jA���A���A�p�A�Q�A���A�l�A��\A�p�A�x�A��TA��A�%A�A�  A���A���A���A���A���A��A��A��A��A��A��mA��mA��mA��mA��mA��mA��`A��`A��HA��/A��/A��#A��A���A���A���A���A���A���A���A���A�ƨA�ƨA�ƨA�ĜA���A��jA��^A��^A��RA��-A���A��A�1'A�
=A���A��A��HA��A���A�ƨA�A��wA��RA��RA��-A��A���A���A���A��7A��A�z�A�t�A�bNA�I�A�+A��A��A��A�JA�A���A���A���A��A��A��TA��/A���A�ĜA��!A���A��+A�ffA�`BA�^5A�\)A�ZA�XA�S�A�K�A�E�A�C�A�C�A�A�A�=qA�=qA�;dA�5?A�1'A�/A�/A�-A�+A�+A�+A�(�A�(�A�(�A�&�A�$�A� �A��A��A��A��A�oA�JA�1A���A���A��A��/A���A��wA��9A���A���A���A���A��\A��A��A�|�A�z�A�x�A�n�A�l�A�r�A�p�A�jA�hsA�dZA�^5A�VA�S�A�O�A�E�A�A�A�;dA�7LA�33A�1'A�-A�+A�+A�$�A��A�{A�
=A���A��A��TA���A���A���A���A��hA��PA��DA��7A��7A��+A��7A��A��A�~�A�|�A�z�A�x�A�t�A�r�A�VA�Q�A�=qA�5?A�-A�&�A�bA��A�JA��^A�ffA��/A�\)A��A�^5A�;dA� �A�ƨA��A���A��wA���A�7LA��`A���A���A���A���A���A���A���A��PA�r�A�p�A�l�A�ffA�\)A�VA�&�A��wA�O�A�9XA�{A�A�l�A�=qA�VA���A���A�Q�A��mA���A��+A�jA�O�A�5?A�/A�&�A���A���A��A��#A���A���A���A���A���A���A���A��^A��A��!A��9A���A���A���A��uA��+A�p�A�t�A�ffA�\)A�ZA�XA�E�A�{A��uA�Q�A�+A�"�A�bA���A��A��RA��!A��wA��9A���A��!A���A���A��7A�~�A�r�A�n�A�O�A��
A��RA�VA��A��;A��jA�S�A��TA��A�ffA�E�A�A���A��A��\A�S�A�+A�A��HA��9A��A�Q�A�1'A�JA��#A���A��A�z�A�jA�VA�A��PA�z�A�\)A�9XA� �A��A�%A��A��yA��HA�ƨA�A���A�^5A�G�A�&�A��A��
A�^5A���A��^A���A��+A�`BA�ZA�K�A�;dA�33A�-A�{A���A���A�ȴA��jA��-A���A��PA�~�A�v�A�5?A��7A�A���A�hsA�
=A��DA�n�A�hsA�\)A�ZA�S�A�O�A�O�A�G�A�9XA�bA���A���A��hA�Q�A��A���A���A�x�A�O�A�-A��A�bA��A��TA��wA��\A�"�A��
A���A��A�S�A��A���A�bNA�=qA�/A�+A��A��A�bA��A��/A���A�A��FA�dZA�9XA���A�\)A�K�A� �A���A���A��7A�C�A�A�A�9XA��A��A�VA�1A�A���A���A���A���A��PA�hsA�XA�O�A�7LA�%A���A���A���A�K�A�\)A�O�A�\)A�Q�A�33A�{A�A��A��A��A�A��PA�ZA�C�A�33A�1A��A���A��RA���A��hA��+A�t�A�XA�C�A�9XA�5?A� �A�%A���A��A��+A��A��+A�z�A�r�A�jA�dZA�dZA�^5A�dZA�`BA�C�A�33A��A��9A���A���A���A��PA��A�~�A�x�A�v�A�n�A�ffA�bNA�E�A�VA�
AƨA|�AK�A+A~�`A~��A~�\A~jA~1'A}�TA}��A}O�A}G�A}�A|��A|�A|=qA|bA{�PA{x�A{C�A{%Az��Az�+AzbNAzE�Az-Az$�Az(�Az�Az1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 A�M�A�M�A�M�A�M�A�O�A�Q�A�Q�A�Q�A�Q�A�O�A�M�A�M�A�S�A�ZA�ZA�XA�ZA�XA��jA�v�A�jA�1A��A��yA��#A���A���A�n�A���A���A�bNA�JA��mA�~�A�O�A�9XA�+A�"�A�JA�A��DA�p�A�XA�7LA��A��wA��7A�~�A�C�A��DA���A�$�A��uA�K�A��
A�G�A�=qA��HA��wA���A�\)A�VA�ĜA��hA�~�A�ȴA��A�hsA�p�A�O�A��/A���A���A�(�A��-A�l�A��A�K�A��7A�bNA��A�Q�A�5?A��
A���A�|�A�VA���A�  A�Q�A���A�G�A��9A�I�A��jA�t�A�VA��RA�z�A�/A33A~�A|�HA{l�AzVAy�Ax$�At�yAs�TAsx�Ar��AqK�Ap�yAo�TAo�AnjAm��Al��Ak�AkS�Aj�Ai��AhA�Ag�
Af�9Ae��Ae%Adr�Ac�AbE�A`{A^�A\��A\�\A\�A[\)AZ �AX�DAW�AWdZAV~�AR9XAP �AO?}AN�AMƨAKAI�AGp�AFbAE�TAE�#AE��AE�^AD�AB(�A>�RA=A<�/A<1A;�TA;��A;��A;`BA;oA:ffA9�hA9+A8��A8VA7��A7+A6�uA6A5ƨA5t�A4I�A3
=A2ffA0-A-�7A+�TA+�A*1A)t�A(�A(�jA(��A(^5A'��A&r�A%XA$��A$jA$1A"�uA"=qA!�-A jA�;A��A1A�PAz�AdZAC�A33A�A��A��A��A�uA-A�^A/A��AVA1'A�At�A/A�9A&�A~�At�A-A�7A��A�DA�A;dA7LA
��A
^5A
bA	��A�`At�At�A��A+A��A n�A E�@���@�E�@�x�@��w@�
=@�-@��-@�dZ@���@��7@�z�@�|�@�\@�@�hs@��/@�33@�!@�=q@�bN@�$�@��;@�l�@��@�n�@�@���@��@�ff@���@ݺ^@�x�@�z�@���@�Ĝ@��
@�l�@�^5@�?}@�z�@���@�@ёh@�Q�@ΰ!@���@�p�@�Ĝ@� �@˝�@�"�@ʗ�@�n�@��@���@�X@���@�Z@š�@��/@�r�@��
@�l�@��y@\@��-@��@���@�?}@��u@�1@��@�"�@�-@�G�@���@�J@�`B@��@��D@��@�"�@�ff@���@��j@��F@���@�%@���@�33@���@�5?@��@�V@��j@�I�@��@�~�@��^@�j@�ƨ@��R@�=q@���@�x�@�%@���@�1'@��F@�\)@�V@�@���@�O�@���@�1'@�b@�|�@�o@���@��@��^@�x�@�/@��/@�Ĝ@���@�33@��@��@���@���@���@�^5@��@��@��#@�p�@���@�z�@�ƨ@�;d@��@��!@���@�ff@�@���@���@�x�@�7L@��/@��9@��u@�j@�I�@�(�@�dZ@�E�@��@���@��@�`B@�&�@��/@���@��j@�Q�@���@��F@�t�@�C�@���@��R@��+@�ff@�E�@�$�@�@��@��@��/@�bN@�1'@�b@��@�S�@�"�@�@��y@�^5@���@�`B@�9X@��@��R@���@���@�^5@���@�G�@�V@��@�j@�@}�-@}�@}`B@}V@|�/@|�j@|�j@|�j@|��@|z�@|j@|9X@{�m@{t�@{S�@{C�@{"�@z��@z~�@z-@zJ@y��@y��@y�#@y��@y�^@y��@y��@yhs@yX@yX@y�@x��@x�@x  @v�@v@u/@t�@t��@s�m@sS�@s33@so@r��@r^5@rM�@r-@rJ@q�#@q�7@q&�@p�`@p�@pr�@pA�@pA�@pbN@pr�@pA�@o�w@n��@nV@m@l�j@k�F@k�@kS�@kC�@k33@ko@j�!@j�@ihs@i�@hbN@g��@f�y@f�+@f@ep�@e�@eV@d�@d�@dZ@d(�@c�F@cS�@ct�@cS�@co@b�@bM�@a�@a��@a�7@`bN@_��@_�w@_\)@^�y@^��@^��@^5?@]��@\��@\�j@\��@\�D@\I�@[�m@[��@[�F@[S�@[33@[@Z�@[@Y��@Y�^@Y��@Y7L@Xr�@Xr�@X1'@W�w@W�P@W;d@W+@W�@Vȴ@Vff@V5?@U�@U/@UV@TZ@T�@T1@S�
@SdZ@So@So@So@So@S@R��@R�\@R�\@R~�@R-@Q�#@Q��@Q7L@Q&�@Q�@P��@PQ�@O|�@OK�@O�@Nȴ@N��@N��@N5?@M�@M?}@L�@L�@L�D@Lz�@LI�@L�@K�F@K33@J�@J��@J^5@JJ@I�^@I��@I7L@H��@H��@Hr�@H1'@G�@G;d@Fv�@E�@E�h@EO�@Dz�@C�m@Ct�@Bn�@A�@A�^@Ax�@A&�@A�@A%@@��@@Ĝ@@A�@?�;@?�;@?�w@>��@>��@>�+@>$�@=�@=O�@=/@=V@<�D@;�
@;��@;o@:�\@:J@9��@9�^@9��@9�7@9X@9G�@97L@9�@9%@8�`@8��@8Ĝ@8��@8�u@8�@8b@7��@7��@7�P@7|�@7\)@7;d@7+@6�y@6ff@5�-@5`B@5�@4��@4�@4�/@4�j@4I�@3��@3��@3�
@3�
@3�
@3�@2�@2�\@2n�@2=q@1��@1�#@1�^@1��@1&�@0��@0�`@0�`@0�9@0bN@0b@/�@/�;@/��@/�w@/��@/�P@/l�@/;d@.��@.ȴ@.��@.E�@.{@.@-�T@-�T@-�T@-��@,�@,�j@,�@,�@,j@,Z@,9X@+��@+dZ@+dZ@+S�@+S�@+S�@+S�@+C�@+33@+"�@+o@+@*�@*�@*�H@*��@*~�@*^5@*�@)��@)7L@(�`@(��@(r�@( �@'��@';d@';d@'+@'�@'
=@&��@&�y@&�y@&�@&��@&E�@&5?@&@%@%�-@%�-@%�h@%?}@$�@$�/@$��@$��@$Z@$I�@$(�@#�m@#��@#C�@"�@"��@"��@"��@"�\@"M�@"�@!�#@!�^@!��@!��@!x�@ ��@ Ĝ@ ��@ �@ bN@ b@
=@��@��@�+@v�@v�@V@$�@�@��@@��@`B@�@�/@�j@�@(�@�@1@�m@ƨ@�F@��@dZ@"�@@�\@n�@n�@=q@��@�#@��@�^@�^@x�@&�@�`@Ĝ@Q�@�;@�P@|�@l�@\)@K�@;d@;d@��@�@�R@��@v�@$�@�@��@�-@��@�h@�@?}@�@�@�/@�j@�@z�@�@��@t�@dZ@dZ@C�@33@@��@^5@=q@=q@-@J@�#@��@x�@7L@��@r�@1'@1'@b@�;@��@l�@�@��@�@��@�+@v�@ff@5?@@��@�-@`B@?}@?}@/@�@��@�/@�@Z@�@�F@dZ@"�@
�!@
�\@
n�@
-@	�^@	��@	X@	&�@��@�9@�u@bN@bN@r�@Q�@  @�;@�@��@l�@�@��@�y@�@�@�@ȴ@�R@V@{@�@?}@�@�j@��@j@�@��@ƨ@��@t�@C�@"�@o@o@�H@�!@~�@M�@=q@�@�@��@��@��@��@�7@hs@G�@7L@&�@�@�@%@%@ �`@ ��@ ĜG�O�A�K�A�M�A�K�A�M�A�M�A�M�A�Q�A�O�A�O�A�M�A�M�A�M�A�M�A�I�A�K�A�M�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�O�A�O�A�O�A�Q�A�Q�A�S�A�S�A�S�A�Q�A�O�A�O�A�S�A�Q�A�Q�A�Q�A�S�A�Q�A�O�A�K�A�I�A�I�A�M�A�O�A�O�A�M�A�M�A�K�A�K�A�K�A�K�A�O�A�VA�VA�XA�XA�XA�XA�XA�ZA�ZA�ZA�XA�ZA�\)A�\)A�XA�XA�XA�XA�XA�VA�ZA�ZA�\)A�ZA�XA�XA�XA�XA�VA�VA�XA�ZA�ZA�\)A�^5A�^5A�`BA�^5A�\)A�\)A�ZA�\)A�ZA�C�A�+A��A��A���A�dZA�hsA�ffA�l�A�A�/A�7LA�K�A�jA���A���A�p�A�Q�A���A�l�A��\A�p�A�x�A��TA��A�%A�A�  A���A���A���A���A���A��A��A��A��A��A��mA��mA��mA��mA��mA��mA��`A��`A��HA��/A��/A��#A��A���A���A���A���A���A���A���A���A�ƨA�ƨA�ƨA�ĜA���A��jA��^A��^A��RA��-A���A��A�1'A�
=A���A��A��HA��A���A�ƨA�A��wA��RA��RA��-A��A���A���A���A��7A��A�z�A�t�A�bNA�I�A�+A��A��A��A�JA�A���A���A���A��A��A��TA��/A���A�ĜA��!A���A��+A�ffA�`BA�^5A�\)A�ZA�XA�S�A�K�A�E�A�C�A�C�A�A�A�=qA�=qA�;dA�5?A�1'A�/A�/A�-A�+A�+A�+A�(�A�(�A�(�A�&�A�$�A� �A��A��A��A��A�oA�JA�1A���A���A��A��/A���A��wA��9A���A���A���A���A��\A��A��A�|�A�z�A�x�A�n�A�l�A�r�A�p�A�jA�hsA�dZA�^5A�VA�S�A�O�A�E�A�A�A�;dA�7LA�33A�1'A�-A�+A�+A�$�A��A�{A�
=A���A��A��TA���A���A���A���A��hA��PA��DA��7A��7A��+A��7A��A��A�~�A�|�A�z�A�x�A�t�A�r�A�VA�Q�A�=qA�5?A�-A�&�A�bA��A�JA��^A�ffA��/A�\)A��A�^5A�;dA� �A�ƨA��A���A��wA���A�7LA��`A���A���A���A���A���A���A���A��PA�r�A�p�A�l�A�ffA�\)A�VA�&�A��wA�O�A�9XA�{A�A�l�A�=qA�VA���A���A�Q�A��mA���A��+A�jA�O�A�5?A�/A�&�A���A���A��A��#A���A���A���A���A���A���A���A��^A��A��!A��9A���A���A���A��uA��+A�p�A�t�A�ffA�\)A�ZA�XA�E�A�{A��uA�Q�A�+A�"�A�bA���A��A��RA��!A��wA��9A���A��!A���A���A��7A�~�A�r�A�n�A�O�A��
A��RA�VA��A��;A��jA�S�A��TA��A�ffA�E�A�A���A��A��\A�S�A�+A�A��HA��9A��A�Q�A�1'A�JA��#A���A��A�z�A�jA�VA�A��PA�z�A�\)A�9XA� �A��A�%A��A��yA��HA�ƨA�A���A�^5A�G�A�&�A��A��
A�^5A���A��^A���A��+A�`BA�ZA�K�A�;dA�33A�-A�{A���A���A�ȴA��jA��-A���A��PA�~�A�v�A�5?A��7A�A���A�hsA�
=A��DA�n�A�hsA�\)A�ZA�S�A�O�A�O�A�G�A�9XA�bA���A���A��hA�Q�A��A���A���A�x�A�O�A�-A��A�bA��A��TA��wA��\A�"�A��
A���A��A�S�A��A���A�bNA�=qA�/A�+A��A��A�bA��A��/A���A�A��FA�dZA�9XA���A�\)A�K�A� �A���A���A��7A�C�A�A�A�9XA��A��A�VA�1A�A���A���A���A���A��PA�hsA�XA�O�A�7LA�%A���A���A���A�K�A�\)A�O�A�\)A�Q�A�33A�{A�A��A��A��A�A��PA�ZA�C�A�33A�1A��A���A��RA���A��hA��+A�t�A�XA�C�A�9XA�5?A� �A�%A���A��A��+A��A��+A�z�A�r�A�jA�dZA�dZA�^5A�dZA�`BA�C�A�33A��A��9A���A���A���A��PA��A�~�A�x�A�v�A�n�A�ffA�bNA�E�A�VA�
AƨA|�AK�A+A~�`A~��A~�\A~jA~1'A}�TA}��A}O�A}G�A}�A|��A|�A|=qA|bA{�PA{x�A{C�A{%Az��Az�+AzbNAzE�Az-Az$�Az(�Az�Az1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B�"B��B��B�B�JB�B�JB��B�xB�rB��B�>B��B��B�DB�8B��B�AB�fB��B�;B�B�mB�B��B��B�B�HB�jB�/B�dB�WBںB�B�B�QB�B՛B҉B҉B��B��B�BȴBĜB��B��BcBZQBG�B@OB(XB�B�2B�B�]B�KB�&B��B�XB��B��B��B�1Bx�Bh�BY�BL�BG�B(�B�B�B\B�B��B��B�'B��B��B�oB�7B��Be,BZQBT�BK�BA�B;dB5?B)�B$B!BSB:B"BBoB��B�5B�,B�]BуB�^B�B�tB�B��B�4B�SB��B�~B��B�oBz�BjBoiBj�Be`BaBYBT�BQBJXBD�B@�B;�B2�B(�B!B1B{B�BPB�B
��B
�B
�B
�WB
ޞB
�vB
�KB
�3B
�qB
�B
�XB
��B
��B
��B
� B
�(B
��B
�+B
|�B
l�B
ffB
c�B
^B
[�B
X�B
V�B
UgB
S[B
QB
K�B
H�B
F?B
C-B
@B
;�B
9XB
6FB
5�B
1[B
-wB
"�B
B
B

	B
 �B	��B	��B	�B	�B	��B	��B	�WB	��B	�B	�;B	�WB	רB	�WB	خB	��B	�2B	�vB	ʌB	�#B	��B	ÖB	ÖB	�B	�B	�B	�dB	�^B	��B	��B	�3B	��B	�B	�qB	��B	�*B	�$B	��B	��B	�@B	��B	�B	�1B	�SB	�{B	��B	��B	�DB	�YB	�GB	�AB	�B	��B	}�B	{B	{B	v�B	t�B	m�B	n�B	l�B	hsB	e`B	e�B	d�B	c�B	`�B	a|B	^�B	]dB	_�B	Z�B	YB	YB	WsB	V9B	U�B	R�B	S[B	R B	P�B	PB	PB	NB	QB	M�B	MB	K�B	K^B	MjB	J�B	K)B	J#B	I�B	H�B	H�B	IB	IRB	G�B	GEB	G�B	GB	FtB	GEB	EB	D�B	F�B	F�B	F�B	FB	F�B	F�B	F�B	GB	GB	F�B	F�B	FtB	FtB	E�B	F?B	J�B	I�B	I�B	I�B	I�B	IRB	IB	I�B	NB	PHB	NpB	PB	O�B	PB	N�B	Q�B	R B	R B	[�B	X�B	X�B	ZB	[�B	^jB	^�B	`vB	bNB	c�B	d�B	jB	k�B	l�B	m]B	l�B	n/B	o5B	p�B	r�B	xB	w�B	xB	x�B	{JB	�4B	�B	�{B	��B	��B	��B	�7B	��B	�	B	�"B	�VB	��B	��B	��B	�{B	��B	�$B	��B	��B	��B	�B	�B	��B	��B	�'B	��B	�*B	�eB	�B	��B	��B	�=B	�wB	��B	��B	��B	��B	�nB	�LB	�0B	��B	�B	��B	��B	�[B	��B	��B	�tB	�zB	�B	�^B	��B	̘B	�jB	��B	�B	��B	�QB	�]B	�jB	��B	ߤB	�HB	� B	� B	�B	��B	�KB	�B	�WB	�B	�vB	�B	�B	��B	��B	��B	�+B	��B	�B	�]B
AB
{B
MB
_B

	B
xB
JB
~B
�B
SB
�B
!�B
*�B
,=B
,�B
,�B
.�B
2�B
7�B
8�B
9�B
=qB
E�B
J�B
K^B
K�B
MjB
N<B
N�B
NpB
NpB
OB
OvB
O�B
PB
Q�B
S[B
S�B
T,B
TaB
VB
WsB
X�B
YKB
Y�B
Y�B
ZB
Z�B
Z�B
[#B
[WB
\]B
\�B
\]B
]�B
]�B
_�B
bB
f�B
kQB
oiB
pB
qvB
v+B
y>B
yrB
zxB
|PB
}�B
~(B
~�B
cB
�4B
��B
��B
��B
��B
��B
��B
�B
�	B
�rB
��B
�B
�eB
�IB
��B
�zB
��B
�0B
�kB
�kB
�B
�qB
�B
��B
�'B
��B
�B
�?B
�B
�XB
��B
��B
�wB
�B
�wB
�HB
��B
��B
ĜB
�mB
ǮB
��B
�dB
�jB
�B
�NB
��B
҉B
�mB
�?B
�?B
�EB
�B
�WB
یB
��B
�jB
ߤB
�BB
�BB
�vB
�B
�B
�B
��B
��B
�2B
�mB
�8B
�B
�B
��B
�WB
�B
�B
�cB
�iB
�;B
��B
�B
�B
�AB
�GB
�MB
�B
�B
�+B
��B
�	B
�>B
��B
�B
�JB
��B
��B
�PB
�B
�PB
��B
�VB
�"B
��B
�(B
�.B
��B;BBB�BB%B�B+B�B�B+B�B	B	�B
	B
�B
�B
�B
�BxB~B�B"B�B\B�B�BbBB�BBuBB�B�B�B7B�B	B~B�B�B!�B#:B#�B$@B$�B$�B$�B%B%zB&�B'�B'�B'�B)�B*�B+B+�B-B,�B,�B,�B-�B.�B/B/�B0�B1�B2�B2aB2�B2�B3�B4B4nB4�B4nB5B4�B5?B5tB5tB5�B6B6zB6�B7B6�B7�B8B8B8�B9�B:^B:�B;dB;dB;dB;�B;�B=�B>B>B>�B>wB>�B?}BA BAUBA�BB�BC-BCaBC�BC�BEBE9BEBE9BEmBF?BF�BGBGBGEBGzBG�BG�BG�BH�BIBI�BI�BJ�BK)BK^BK^BK�BK^BK�BLdBL�BLdBLdBMBMBM6BN�BOBOBBOBOBBOBBOvBOBOBOBBOvBOvBOvBO�BO�BPHBPHBPBQBR�BR�BR�BS&BS�BT,BT�BT,BT,BT,BT,BS�BT,BTaBT,BTaBT�BU�BU�BV�BVmBVmBVBV9BVBV�BW
BWsBXBXyBXEBX�BYBYKBZ�B[WB[�B[�B[�B\)B\�B]/B]�B]dB]/B\�B]�B]�B]dB]dB]dB]dB]�B_�B`B_�B_�B_�B_�B_�B`B`BB`�B`�B`�BaHBa�Ba�Ba�Ba�Bc Bb�Bc Bc BcTBcTBcTBc�BdZBd�Be�Be�Be`Be�Be�Be�Bf2Be�Be�BffBf�Bf�Bf�Bg�Bh>Bh�BiBiBiDBiDBiBiBiyBiDBiDBiyBi�BjKBjBjKBjBjBjBj�Bj�Bj�Bj�Bk�Bk�BkQBk�Bl�Bl�Bl�BlWBlWBl�Bl"Bl�Bl�Bm]Bm)Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bm�BncBm�Bm�Bm�Bm�BncBncBncBn�Bn�Bo BoiBn�Bn�Bn�Bo Bo BoiBo�Bo�BoiBo5BoiBoiBo5Bo5Bo�Bo�BpBp;Bp;Bp�BqBp�Bp�BqABqBqABqvBrBrGBr�Br�Br|Br|Br�BsMBsMBs�Bs�Bs�Bt�Bt�Bt�Bt�Bt�Bt�BtBtTBt�Bt�Bu�Bu�Bu�Bv+Bv+Bv+Bv�Bv�Bw2Bw2Bw2Bw�BwfBw2Bw2BwfBw�Bw�BxBw�Bw�Bx8BxlBx�Bx�Bx�Bx�By	By	By>ByrBy�ByrBy�By>By>ByrBy�By�B��B��B�cB�]B��B�cB��B��B��B�.B�]B��B�(B�.B��B�B��B��B�B��B�"B��B��B��B��B��B�PB��B��B��B��B��B�B��B��B�JB��B��B��B�B��B�B�PB�B�rB�rB�rB�	B�JB�B�DB�B�	B�	B��B��B��B��B�B��B�xB�>B�	B��B�B��B��B��B��B�DB��B��B�rB��B�>B��B��B��B�B�JB�xB�B�JB��B�JB��B�>B��B�rB��B��B�B�B�DB��B�>B��B��B�%B�JB%B�BB��B�B�EB�fB��B�B�]B��B��B��B�	BB;B�B��B��B�B�VB�B�B��B��B�B�GB�|B��B�;B�AB�B�GB�B�vB��B�B�vB��B�oB��B�iB�/B�"B�B�>B�8B�B�8B��B�mB�8B�2B��B��B��B�B�B�KB�B�B��B�sB�
B�>B��B�B��B�DB��B�2B��B��B�B�B�B��B�TB�NB�B�NB�HB�B�B�B�vB�pBޞB�B�HB�|B�;B�jB��B�B�B��B�dB��BݘBޞB�BܒB�BޞB�;B��B�B�vB�]B��B�)B�)B�]BچB��B��BیB�#B�#BچBںBںBیBٴB�B�KB�B��B�KB�B�KB�EBخB�yB�KB�B�KB��BٴBںB��BںBںBںB�QB��B�/B�QB�B��BچB֡B�?B��B�?B�gB��B��B��B��B��BѷBуB҉BѷB�NB�TBбBбB�BӏB�B�gB�B՛B՛B��BԕB��B� B҉B՛B��B�gB�B�&B��BҽB��B�^B�jB˒B��BȀBǮB�B�B�EB�?BǮB�9B�zB�9B��B�'B�UB�B�6B�B�B��B��B�tB�3B�jB��B�B�B�B�B�uBxlBt�B�ABrGBb�Bd�B\�Bf�B^5BQBJ#BG�BGBGEBGEBF?BGEBE�BAUBA B?HB=qB<�BCaB@�B;�B&B)�B+B�B�B=B�BB
=B�B�fB��B��B�2B�>B��B�MB�(B�5B�;B��B�B��B��B�AB�B�]B�/B�/B�WB�"B��B�B��B�B�>B�WB�B��B��B��B��B�|B�HB�DBٴB� B�B�B�<B�B�B̘BƨBƨB�tB˒B��BƨB��BɆB��B��B��B�UB��B�aB��B��B��B��B�B�=B�	B��B�oB�\B��B��B��B��B��B�B~�B~�B��Bu%BsMBq�Bv�BpoBiyBc�BcTBb�Bp;Bf2BW
B]�BW?BT�BQNBQ�BNpBK�BK^BN�BGBL0BNB?�B?HBJ�BMjB=qB4�B(XB&�B$�B&B!B!B!BBeBB�BYB�B�BbB�B�B	�B�B�B\B�B	lB�+B��B�B�B�B�BݘB�dBܒBچBچB�QBܒB�pB��B�NB� B�vB�aB�UB�)B��B��B��B��B�B�zB�B��B�0B�B�tB�@B��B�qB��B�eB�B��B��B��B��B�PB�oB��B�B��B�GB��B�Bt�BkQBl�Bm�Bl"Bc�BffBUgBc�Be�Bg�BX�BZ�BW
BX�BU�B^jBT,B^�BMjBRTBQ�BL0BJXBXEBF�BIRBK^BH�B<6BEB6�B>�B?�B:�B8RB;�B:^B4B=�B49B=B&�B3hB3hB*�B0�B,=B&�B+6B%�B&�B'�B!�B#nB�B%�B�B!�B"4B�BYB�B7B�B�BB:B�B�B�B B B�B	lB	lB	7B	7B�B�B�BGB�B�BABB%BSB��B��B��B��B�B�MB�B�iB�5B�iB�)B�iB��B�&B�8B�&B��B�jB�yB�cB�yB��BچBԕB�B�B� B�<B�jB��B͟B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202103261700362021032617003620210326170036202103261700362021032617003620210326170036SI  SI  ARFMARFM                                                                                                                                                2021013008580020210130085800IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021020908005220210209080052QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021020908005220210209080052QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021032510164520210325101645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021032617005120210326170051IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V03                       ARGO_for_DMQC Climatology Version 2020V03                       2021032617005120210326170051IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021032617005120210326170051IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                